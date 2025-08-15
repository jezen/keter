{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | A light-weight, minimalistic reverse HTTP proxy.
module Keter.Proxy
    ( reverseProxy
    , makeSettings
    , ProxySettings(..)
    ) where

import Blaze.ByteString.Builder (copyByteString, toByteString)
import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString)
import Control.Applicative ((<|>))
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger
import Control.Monad.Reader (ask)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as S
import Data.ByteString.Char8 qualified as S8
import Data.CaseInsensitive qualified as CI
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text as T (Text, pack, unwords)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Vector qualified as V
import Data.Version (showVersion)
import GHC.Exts (fromString)
import Keter.Common
import Keter.Config
import Keter.Config.Middleware (MiddlewareConfig, processMiddlewareIO)
import Keter.Context
import Keter.HostManager qualified as HostMan
import Keter.Rewrite qualified as Rewrite
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.ReverseProxy
       ( LocalWaiProxySettings
       , ProxyDest(ProxyDest)
       , SetIpHeader(..)
       , WaiProxyResponse(..)
       , defaultLocalWaiProxySettings
       , defaultWaiProxySettings
       , setLpsTimeBound
       , waiProxyToSettings
       , wpsGetDest
       , wpsOnExc
       , wpsSetIpHeader
       )
import Network.HTTP.Types
       ( mkStatus
       , status200
       , status301
       , status302
       , status303
       , status307
       , status404
       , status502
       )
import Network.TLS qualified as TLS
import Network.TLS.SessionManager qualified as TLSSession
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
       (defaultFileServerSettings, ssListing, staticApp)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import Network.Wai.Middleware.Gzip (GzipFiles(..), GzipSettings(..), def, gzip)
import Paths_keter qualified as Pkg
import Prelude hiding (FilePath, (++))
import System.Directory qualified as Dir
import System.FilePath (FilePath)
import System.IO.Unsafe (unsafePerformIO)
import WaiAppStatic.Listing (defaultListing)
import qualified Data.ByteString.Lazy as LBS

--------------------------------------------------------------------------------
-- Compiled middleware caching (per vhost + middleware list)
--
-- Why cache?
--   - Some middlewares are stateful (e.g., rate limiters) and should persist
--     across requests to maintain counters/buckets.
--   - Compiling the chain for every request is wasteful.
-- Isolation:
--   - Key includes the Host and the exact middleware list (JSON-encoded) so
--     each vhost gets its own isolated state.

type MWCacheKey = (ByteString, ByteString) -- (Host header, JSON-encoded middleware list)

{-# NOINLINE middlewareCache #-}
middlewareCache :: IORef (HM.HashMap MWCacheKey Wai.Middleware)
middlewareCache = unsafePerformIO $ newIORef HM.empty

-- | Look up or build the middleware chain for a given vhost and config list.
getOrBuildMiddleware :: ByteString -> [MiddlewareConfig] -> IO Wai.Middleware
getOrBuildMiddleware host cfgs = do
  let key :: MWCacheKey
      key = (host, LBS.toStrict (Aeson.encode cfgs))
  cache <- readIORef middlewareCache
  case HM.lookup key cache of
    Just mw -> pure mw
    Nothing -> do
      mw <- processMiddlewareIO cfgs
      atomicModifyIORef' middlewareCache $ \m -> (HM.insert key mw m, ())
      pure mw

--------------------------------------------------------------------------------

data ProxySettings = MkProxySettings
  { -- | Mapping from virtual hostname to port number.
    psHostLookup     :: ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))
  , psManager        :: !Manager
  , psIpFromHeader   :: Bool
  , psConnectionTimeBound :: Int
  , psHealthcheckPath :: !(Maybe ByteString)
  , psUnknownHost    :: ByteString -> ByteString
  , psMissingHost    :: ByteString
  , psProxyException :: ByteString
  }

makeSettings :: HostMan.HostManager -> KeterM KeterConfig ProxySettings
makeSettings hostman = do
    KeterConfig{..} <- ask
    psManager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    -- Preload response bodies once (fast path for error/edge cases) so
    -- the proxy layers (outer and inner) can render without I/O on error.
    psMissingHost <- taggedReadFile "missing-host-response-file" kconfigMissingHostResponse defaultMissingHostBody id
    psUnknownHost <- taggedReadFile "unknown-host-response-file" kconfigUnknownHostResponse defaultUnknownHostBody const
    psProxyException <- taggedReadFile "proxy-exception-response-file" kconfigProxyException defaultProxyException id
    -- Calculate microseconds; the config is in milliseconds.
    let psConnectionTimeBound = kconfigConnectionTimeBound * 1000
    let psIpFromHeader = kconfigIpFromHeader
    let psHealthcheckPath = encodeUtf8 <$> kconfigHealthcheckPath
    pure $ MkProxySettings{..}
    where
        psHostLookup = HostMan.lookupAction hostman . CI.mk

taggedReadFile :: Text -> Maybe FilePath -> r -> (ByteString -> r) -> KeterM KeterConfig r
taggedReadFile _    Nothing    fallback _               = pure fallback
taggedReadFile tag (Just file) fallback processContents = do
  isExist <- liftIO $ Dir.doesFileExist file
  if isExist then liftIO (S.readFile file) <&> processContents else do
    wd <- liftIO Dir.getCurrentDirectory
    logWarnN . T.unwords $ ["could not find", tag, "on path", quote file, "with working dir", quote wd]
    return fallback
  where
    quote = ("'" <>) . (<> "'") . fromString

reverseProxy :: ListeningPort -> KeterM ProxySettings ()
reverseProxy listener = do
  settings <- ask
  let (run, isSecure) =
          case listener of
              LPInsecure host port ->
                  (liftIO . Warp.runSettings (warp host port), False)
              LPSecure host port cert chainCerts key session ->
                  (liftIO . WarpTLS.runTLS
                      (connectClientCertificates (psHostLookup settings) session $ WarpTLS.tlsSettingsChain
                          cert
                          (V.toList chainCerts)
                          key)
                      (warp host port), True)
  -- Gzip is still applied outermost; middleware chains are per-vhost under that.
  withClient isSecure >>= run . gzip def{gzipFiles = GzipPreCompressed GzipIgnore}
  where
    warp host port = Warp.setHost host $ Warp.setPort port Warp.defaultSettings

connectClientCertificates :: (ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))) -> Bool -> WarpTLS.TLSSettings -> WarpTLS.TLSSettings
connectClientCertificates hl session s =
    let
        newHooks = WarpTLS.tlsServerHooks s
        -- todo: add nested lookup
        newOnServerNameIndication (Just n) =
             maybe mempty snd <$> hl (S8.pack n)
        newOnServerNameIndication Nothing =
             return mempty -- we could return default certificate here
    in
        s { WarpTLS.tlsServerHooks = newHooks{TLS.onServerNameIndication = newOnServerNameIndication}
          , WarpTLS.tlsSessionManagerConfig = if session then Just TLSSession.defaultConfig else Nothing }

withClient :: Bool -- ^ Is the outer listener secure (HTTPS)?
           -> KeterM ProxySettings Wai.Application
withClient isSecure = do
    cfg@MkProxySettings{..} <- ask
    let useHeader = psIpFromHeader
        -- Why thread 'useHeader' through?
        --   - Consistency: both outer (this) and inner (PAPort) proxy layers
        --     set wpsSetIpHeader the same way, ensuring the correct client IP
        --     is propagated/observed (e.g., for rate limiting).
    withRunInIO $ \rio ->
        pure $ waiProxyToSettings
           (error "First argument to waiProxyToSettings forced, even thought wpsGetDest provided")
           defaultWaiProxySettings
            { wpsSetIpHeader =
                if useHeader
                    then SIHFromHeader
                    else SIHFromSocket
            ,  wpsGetDest = Just (getDest cfg useHeader)
            ,  wpsOnExc =
                 -- Why pass onExceptBody (preloaded) to the handler?
                 --   - So the exception path is fast and pure-IO, without
                 --     re-entering KeterM or reading files again.
                 handleProxyException (\app e -> rio $ logException app e) psProxyException
            } psManager
  where
    logException :: Wai.Request -> SomeException -> KeterM ProxySettings ()
    logException a b = logErrorN $ pack $
      "Got a proxy exception on request " <> show a <> " with exception "  <> show b

    getDest :: ProxySettings -> Bool -> Wai.Request -> IO (LocalWaiProxySettings, WaiProxyResponse)
    -- Respond to healthchecks regardless of Host header presence/value.
    getDest MkProxySettings{..} _ req
      | psHealthcheckPath == Just (Wai.rawPathInfo req)
      = return (defaultLocalWaiProxySettings, WPRResponse healthcheckResponse)
    -- Inspect Host header to determine which App to proxy to.
    getDest cfg@MkProxySettings{..} useHeader req =
        case Wai.requestHeaderHost req of
            Nothing -> do
              return (defaultLocalWaiProxySettings, WPRResponse $ missingHostResponse psMissingHost)
            Just host -> processHost cfg useHeader req host

    processHost :: ProxySettings -> Bool -> Wai.Request -> S.ByteString -> IO (LocalWaiProxySettings, WaiProxyResponse)
    processHost cfg@MkProxySettings{..} useHeader req host = do
        -- First try full host; fallback to host without :port.
        mport <- liftIO $ do
            mport1 <- psHostLookup host
            case mport1 of
                Just _ -> return mport1
                Nothing -> do
                    let host' = S.takeWhile (/= 58) host
                    if host' == host
                        then return Nothing
                        else psHostLookup host'
        case mport of
            Nothing -> do -- Unknown host
              return (defaultLocalWaiProxySettings, WPRResponse $ unknownHostResponse host (psUnknownHost host))
            Just ((action, requiresSecure), _)
                | requiresSecure && not isSecure -> performHttpsRedirect cfg host req
                | otherwise -> performAction psManager psProxyException isSecure useHeader psConnectionTimeBound host req action

    performHttpsRedirect MkProxySettings{..} host =
        return . (addjustGlobalBound psConnectionTimeBound Nothing,) . WPRResponse . redirectApp config
      where
        host' = CI.mk $ decodeUtf8With lenientDecode host
        config = RedirectConfig
            { redirconfigHosts = mempty
            , redirconfigStatus = 301
            , redirconfigActions = V.singleton $ RedirectAction SPAny
                                 $ RDPrefix True host' Nothing
            , redirconfigSsl = SSLTrue
            }

-- FIXME This is a workaround for
-- https://github.com/snoyberg/keter/issues/29. After some research, it
-- seems like Warp is behaving properly here. I'm still not certain why the
-- http call (from http-conduit) inside waiProxyToSettings could ever block
-- infinitely without the server it's connecting to going down, so that
-- requires more research. Meanwhile, this prevents the file descriptor
-- leak from occurring.
addjustGlobalBound :: Int -> Maybe Int -> LocalWaiProxySettings
addjustGlobalBound bound to = go `setLpsTimeBound` defaultLocalWaiProxySettings
  where
    go = case to <|> Just bound of
           Just x | x > 0 -> Just x
           _              -> Nothing

-- | Perform the stanza action, applying IO-aware middlewares where available.
-- Parameters of note:
--   - onExceptBody: preloaded proxy-exception body (ByteString). We pass it
--     down so the inner (PAPort) proxy can render error pages in pure IO,
--     without re-entering KeterM or doing file I/O.
--   - useHeader: whether to trust X-Forwarded-For for client IP (from config).
--     We apply this consistently to outer and inner proxy layers so middlewares
--     (like the rate limiter) observe the correct client identity.
performAction :: Manager
              -> ByteString               -- ^ onExceptBody: proxy exception body
              -> Bool                     -- ^ isSecure: outer listener is HTTPS?
              -> Bool                     -- ^ useHeader: trust X-Forwarded-For?
              -> Int                      -- ^ global connection time bound (us)
              -> ByteString               -- ^ host (for middleware cache key)
              -> Wai.Request
              -> ProxyActionRaw
              -> IO (LocalWaiProxySettings, WaiProxyResponse)
performAction psManager onExceptBody isSecure useHeader globalBound host req = \case
  (PAPort port mids tbound) -> do
    -- Build an inner reverse-proxy app that always targets 127.0.0.1:port,
    -- then wrap it with the IO-aware middleware chain for this webapp stanza.
    --
    -- Why set X-Forwarded-Proto here?
    --   - The inner hop is HTTP to localhost, but the client might be using
    --     HTTPS at the edge. We normalize X-Forwarded-Proto so the app and
    --     middlewares see the correct original scheme.
    let innerApp :: Wai.Application
        innerApp =
          waiProxyToSettings
            (error "inner default app forced, even though wpsGetDest is provided")
            defaultWaiProxySettings
              { wpsSetIpHeader =
                  if useHeader then SIHFromHeader else SIHFromSocket
              , wpsGetDest = Just $ \r -> do
                  let protocol = if isSecure then "https" else "http"
                      r' = r { Wai.requestHeaders =
                                ("X-Forwarded-Proto", protocol) : Wai.requestHeaders r
                             }
                  pure (defaultLocalWaiProxySettings, WPRModifiedRequest r' (ProxyDest "127.0.0.1" port))
              , wpsOnExc =
                  -- Inner handler: use the same preloaded body; we skip logging
                  -- here to avoid duplicate logs and because we are in plain IO.
                  handleProxyException (\_ _ -> pure ()) onExceptBody
              } psManager
    mw <- getOrBuildMiddleware host mids
    pure ( addjustGlobalBound globalBound tbound
         , WPRApplication (mw innerApp)
         )

  (PAStatic StaticFilesConfig {..}) -> do
    let baseApp = staticApp (defaultFileServerSettings sfconfigRoot)
          { ssListing =
              if sfconfigListings
                  then Just defaultListing
                  else Nothing
          }
    mw <- getOrBuildMiddleware host sfconfigMiddleware
    return ( addjustGlobalBound globalBound sfconfigTimeout
           , WPRApplication (mw baseApp)
           )

  (PARedirect config) ->
    return (addjustGlobalBound globalBound Nothing, WPRResponse $ redirectApp config req)

  (PAReverseProxy config rpconfigMiddleware tbound) -> do
       let baseApp = Rewrite.simpleReverseProxy psManager config
       mw <- getOrBuildMiddleware host rpconfigMiddleware
       return ( addjustGlobalBound globalBound tbound
              , WPRApplication (mw baseApp)
              )

redirectApp :: RedirectConfig -> Wai.Request -> Wai.Response
redirectApp RedirectConfig {..} req =
    V.foldr checkAction noAction redirconfigActions
  where
    checkAction (RedirectAction SPAny dest) _ = sendTo $ mkUrl dest
    checkAction (RedirectAction (SPSpecific path) dest) other
        | encodeUtf8 path == Wai.rawPathInfo req = sendTo $ mkUrl dest
        | otherwise = other

    noAction = Wai.responseBuilder
        status404
        [("Content-Type", "text/plain")]
        (copyByteString "File not found")

    sendTo url = Wai.responseBuilder
        status
        [("Location", url)]
        (copyByteString url)

    status =
        case redirconfigStatus of
            301 -> status301
            302 -> status302
            303 -> status303
            307 -> status307
            i   -> mkStatus i $ S8.pack $ show i

    mkUrl (RDUrl url) = encodeUtf8 url
    mkUrl (RDPrefix isSecure host mport) = S.concat
        [ if isSecure then "https://" else "http://"
        , encodeUtf8 $ CI.original host
        , case mport of
            Nothing -> ""
            Just port
                | isSecure && port == 443 -> ""
                | not isSecure && port == 80 -> ""
                | otherwise -> S8.pack $ ':' : show port
        , Wai.rawPathInfo req
        , Wai.rawQueryString req
        ]

-- | Render a configured proxy-exception body.
-- We pass the body from makeSettings so this path is IO-only (no monadic
-- context or file I/O), and we can reuse it for both outer and inner proxies.
handleProxyException :: (Wai.Request -> SomeException -> IO ()) -> ByteString -> SomeException -> Wai.Application
handleProxyException handleException onexceptBody except req respond = do
  handleException req except
  respond $ missingHostResponse onexceptBody

healthcheckResponse :: Wai.Response
healthcheckResponse = Wai.responseBuilder
    status200
    [("Content-Type", "text/plain; charset=utf-8")]
    $ "Keter " <> (copyByteString . S8.pack . showVersion) Pkg.version
               <> " is doing okay!\n"

defaultProxyException :: ByteString
defaultProxyException = "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>There was a proxy error, check the keter logs for details.</p></body></html>"

defaultMissingHostBody :: ByteString
defaultMissingHostBody = "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>You did not provide a virtual hostname for this request.</p></body></html>"

-- | Error, no host found in the header
missingHostResponse :: ByteString -> Wai.Response
missingHostResponse missingHost = Wai.responseBuilder
    status502
    [("Content-Type", "text/html; charset=utf-8")]
    $ copyByteString missingHost

defaultUnknownHostBody :: ByteString -> ByteString
defaultUnknownHostBody host =
  "<!DOCTYPE html>\n<html><head><title>Welcome to Keter</title></head><body><h1>Welcome to Keter</h1><p>The hostname you have provided, <code>"
  <> escapeHtml host <> "</code>, is not recognized.</p></body></html>"

-- | We found a host in the header, but we don't know about the host asked for.
unknownHostResponse :: ByteString -> ByteString -> Wai.Response
unknownHostResponse host body = Wai.responseBuilder
    status404
    [("Content-Type", "text/html; charset=utf-8"),
     ("X-Forwarded-Host",
      -- if an attacker manages to insert line breaks somehow,
      -- this is also vulnerable.
      escapeHtml host
     )]
    (copyByteString body)

escapeHtml :: ByteString -> ByteString
escapeHtml = toByteString . fromHtmlEscapedByteString
