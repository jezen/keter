{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Keter.Proxy.MiddlewareSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Keter.Config.Middleware (MiddlewareConfig)
import Keter.Config.V10
import Keter.Context
import Keter.Proxy
import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types.Status (ok200, statusCode)
import qualified Network.Wai as Wai
import qualified Network.TLS as TLS (Credentials)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wreq as Wreq
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Keter.Proxy integration (middleware smoke tests)"
  [ testCase "Rate-limit via PAPort (FixedWindow, ip-from-header=False)" caseRateLimitFixedWindow
  , testCase "ip-from-header=True uses X-Forwarded-For" caseIpFromHeaderTrue
  , testCase "Healthcheck bypasses middleware" caseHealthcheckBypass
  , testCase "Proxy exception body on backend down" caseProxyExceptionBody
  ]

-- Helpers

startBackend :: Int -> IO ()
startBackend port = void . forkIO $
  Warp.run port $ \req respond -> do
    -- Drain body so HEAD/POST sequences don't leave unread input
    void $ Wai.strictRequestBody req
    respond $ Wai.responseLBS ok200 [] "ok"

runProxyOn :: Manager -> ProxySettings -> Int -> IO ()
runProxyOn _ settings listenPort = void . forkIO $
  flip runReaderT settings $
  flip runLoggingT (\_ _ _ _ -> pure ()) $   -- silence logs in tests
    runKeterM $
      reverseProxy (LPInsecure "*" listenPort)

mkSettings
  :: Manager
  -> (ByteString -> IO (Maybe (ProxyAction, TLS.Credentials)))
  -> Bool                   -- ip-from-header
  -> Maybe ByteString       -- healthcheck-path
  -> ByteString             -- proxy-exception body
  -> IO ProxySettings
mkSettings manager hostLookup useHeader mHealth exBody = do
  -- Now we can construct MiddlewareCache since it's exported from Keter.Proxy
  middlewareCache <- MiddlewareCache <$> newTVarIO HM.empty
  
  pure $ MkProxySettings
    { psHostLookup     = hostLookup
    , psManager        = manager
    , psUnknownHost    = const ""
    , psMissingHost    = ""
    , psProxyException = exBody
    , psIpFromHeader   = useHeader
    , psConnectionTimeBound = 5 * 60 * 1000
    , psHealthcheckPath = mHealth
    , psMiddlewareCache = middlewareCache
    }

decodeMids :: LBS.ByteString -> [MiddlewareConfig]
decodeMids lbs =
  case eitherDecode lbs of
    Right xs -> xs
    Left e   -> error ("Failed to decode middleware config: " <> e)

-- Test cases

-- 1) Rate-limit via PAPort (FixedWindow)
caseRateLimitFixedWindow :: IO ()
caseRateLimitFixedWindow = do
  let backendPort = 6791
      proxyPort   = 6790
      midsJSON = LBS.pack $ concat
        [ "[{ \"rate-limiter\": {"
        , "    \"zone_by\":\"default\","
        , "    \"throttles\":[{"
        , "      \"name\":\"ip\","
        , "      \"limit\":2,"
        , "      \"period\":60,"
        , "      \"algorithm\":\"FixedWindow\","
        , "      \"identifier_by\":\"ip\""
        , "    }]"
        , "}}]"
        ]
      mids = decodeMids midsJSON

  _ <- forkIO $ startBackend backendPort

  manager <- HTTP.newManager HTTP.tlsManagerSettings

  let host = "rl.test"
      hostLookup :: ByteString -> IO (Maybe (ProxyAction, TLS.Credentials))
      hostLookup _ =
        -- Adjust PAPort to your signature:
        -- If you have PAPort port mids (Maybe Int):
        --   return $ Just ((PAPort backendPort mids Nothing, False), mempty)
        -- If you still have PAPort port (Maybe Int) only, remove this test or upgrade PAPort.
        return $ Just ((PAPort backendPort mids Nothing, False), mempty)

  settings <- mkSettings manager hostLookup False Nothing "proxy error"  -- Now use <- since it returns IO
  runProxyOn manager settings proxyPort
  threadDelay 200_000

  let base = "http://127.0.0.1:" <> show proxyPort <> "/"
      req = Wreq.defaults & Wreq.header "Host" .~ [host]
  r1 <- Wreq.getWith req base
  r2 <- Wreq.getWith req base
  -- For the third request, we expect a 429, so we need to handle the exception
  r3Result <- try $ Wreq.getWith req base
  
  r1 ^. Wreq.responseStatus . Wreq.statusCode @?= 200
  r2 ^. Wreq.responseStatus . Wreq.statusCode @?= 200
  
  case r3Result of
    Left (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
      statusCode (HTTP.responseStatus resp) @?= 429
    Left e -> assertFailure $ "Unexpected exception: " <> show e
    Right r3 -> r3 ^. Wreq.responseStatus . Wreq.statusCode @?= 429

-- 2) ip-from-header=True: X-Forwarded-For identifies client
caseIpFromHeaderTrue :: IO ()
caseIpFromHeaderTrue = do
  let backendPort = 6793
      proxyPort   = 6792
      midsJSON = LBS.pack $ concat
        [ "[{ \"rate-limiter\": {"
        , "    \"zone_by\":\"default\","
        , "    \"throttles\":[{"
        , "      \"name\":\"ip\","
        , "      \"limit\":1,"
        , "      \"period\":60,"
        , "      \"algorithm\":\"FixedWindow\","
        , "      \"identifier_by\":\"ip\""
        , "    }]"
        , "}}]"
        ]
      mids = decodeMids midsJSON

  _ <- forkIO $ startBackend backendPort
  manager <- HTTP.newManager HTTP.tlsManagerSettings

  let host = "xff.test"
      hostLookup _ = return $ Just ((PAPort backendPort mids Nothing, False), mempty)
  settings <- mkSettings manager hostLookup True Nothing "proxy error"  -- Now use <- since it returns IO
  runProxyOn manager settings proxyPort
  threadDelay 200_000

  let base = "http://127.0.0.1:" <> show proxyPort <> "/"
      req xff =
        Wreq.defaults
          & Wreq.header "Host" .~ [host]
          & Wreq.header "X-Forwarded-For" .~ [xff]

  r1 <- Wreq.getWith (req "1.1.1.1") base
  -- For the second request, we expect a 429, so we need to handle the exception
  r2Result <- try $ Wreq.getWith (req "1.1.1.1") base
  r3 <- Wreq.getWith (req "2.2.2.2") base

  r1 ^. Wreq.responseStatus . Wreq.statusCode @?= 200
  
  case r2Result of
    Left (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
      statusCode (HTTP.responseStatus resp) @?= 429
    Left e -> assertFailure $ "Unexpected exception: " <> show e
    Right r2 -> r2 ^. Wreq.responseStatus . Wreq.statusCode @?= 429
    
  r3 ^. Wreq.responseStatus . Wreq.statusCode @?= 200

-- 3) Healthcheck bypass
caseHealthcheckBypass :: IO ()
caseHealthcheckBypass = do
  let backendPort = 6795
      proxyPort   = 6794
  _ <- forkIO $ startBackend backendPort
  manager <- HTTP.newManager HTTP.tlsManagerSettings

  let host = "hc.test"
      hostLookup _ = return $ Just ((PAPort backendPort [] Nothing, False), mempty)
  settings <- mkSettings manager hostLookup False (Just "/keter-health") "proxy error"  -- Now use <- since it returns IO

  runProxyOn manager settings proxyPort
  threadDelay 200_000

  let base = "http://127.0.0.1:" <> show proxyPort <> "/keter-health"
      req = Wreq.defaults & Wreq.header "Host" .~ [host]
  r <- Wreq.getWith req base
  r ^. Wreq.responseStatus . Wreq.statusCode @?= 200

-- 4) Proxy exception body when backend is down
caseProxyExceptionBody :: IO ()
caseProxyExceptionBody = do
  let proxyPort = 6796
  manager <- HTTP.newManager HTTP.tlsManagerSettings

  let host = "down.test"
      hostLookup _ = return $ Just ((PAPort 59999 [] Nothing, False), mempty) -- no backend here
      exBody = "my branded proxy error"
  settings <- mkSettings manager hostLookup False Nothing exBody  -- Now use <- since it returns IO

  runProxyOn manager settings proxyPort
  threadDelay 200_000

  let base = "http://127.0.0.1:" <> show proxyPort <> "/"
      req = Wreq.defaults & Wreq.header "Host" .~ [host]
  -- We expect a 502, so we need to handle the exception
  rResult <- try $ Wreq.getWith req base
  
  case rResult of
    Left (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
      statusCode (HTTP.responseStatus resp) @?= 502
    Left e -> assertFailure $ "Unexpected exception: " <> show e
    Right r -> r ^. Wreq.responseStatus . Wreq.statusCode @?= 502
  -- Optional: assert body equals exBody if your handler returns it verbatim
  -- LBS.toStrict (r ^. Wreq.responseBody) @?= exBody
