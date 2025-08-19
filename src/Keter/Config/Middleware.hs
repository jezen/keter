{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Keter.Config.Middleware where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Prelude

import Control.Arrow ((***))
import Control.Monad

-- various Middlewares
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.Local (local)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.String (fromString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Keter.Aeson.KeyHelper as AK (empty, toKey, toList, toText)

-- Rate limiter (updated API: buildEnvFromConfig + buildRateLimiterWithEnv)
import Keter.RateLimiter.WAI
  ( RateLimiterConfig(..)
  , buildEnvFromConfig
  , buildRateLimiterWithEnv
  )
--------------------------------------------------------------------------------
-- Middleware config + RateLimiter

data MiddlewareConfig
  = AcceptOverride
  | Autohead
  | Jsonp
  | MethodOverride
  | MethodOverridePost
  | AddHeaders ![(S.ByteString, S.ByteString)]
  | BasicAuth !String ![(S.ByteString, S.ByteString)] -- Realm [(username,password)]
  | Local !Int !L.ByteString                           -- Status Message
  | RateLimiter !RateLimiterConfig                     -- New rate limiter
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- JSON parsing/encoding

instance FromJSON MiddlewareConfig where
  parseJSON (String "accept-override"     ) = pure AcceptOverride
  parseJSON (String "autohead"            ) = pure Autohead
  parseJSON (String "jsonp"               ) = pure Jsonp
  parseJSON (String "method-override"     ) = pure MethodOverride
  parseJSON (String "method-override-post") = pure MethodOverridePost
  parseJSON (Object o) =
    case AK.toList o of
      [("basic-auth", Object o')] ->
        BasicAuth  <$> o' .:? "realm" .!= "keter"
                   <*> (map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8)
                       . AK.toList <$> o' .:? "creds" .!= AK.empty)

      [("headers", Object _ )] ->
        AddHeaders . map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8)
          . AK.toList <$> o .:? "headers" .!= AK.empty

      [("local", Object o')] ->
        Local <$> o' .:? "status" .!= 401
              <*> (TL.encodeUtf8 <$> o' .:? "message"
                   .!= "Unauthorized Accessing from Localhost ONLY")

      [("rate-limiter", v)] -> RateLimiter <$> parseJSON v

      _ -> mzero
  parseJSON _ = mzero

instance ToJSON MiddlewareConfig where
  toJSON AcceptOverride     = "accept-override"
  toJSON Autohead           = "autohead"
  toJSON Jsonp              = "jsonp"
  toJSON MethodOverride     = "method-override"
  toJSON MethodOverridePost = "method-override-post"
  toJSON (BasicAuth realm cred) =
    object [ "basic-auth" .= object
              [ "realm" .= realm
              , "creds" .= object
                  (map ((AK.toKey . T.decodeUtf8) *** (String . T.decodeUtf8)) cred)
              ] ]
  toJSON (AddHeaders headers) =
    object [ "headers" .= object
              (map ((AK.toKey . T.decodeUtf8) *** String . T.decodeUtf8) headers) ]
  toJSON (Local sc msg) =
    object [ "local" .= object [ "status" .= sc
                               , "message" .= TL.decodeUtf8 msg ] ]
  toJSON (RateLimiter rl) = object [ "rate-limiter" .= toJSON rl ]

--------------------------------------------------------------------------------
-- Building middleware (IO-aware)

-- Build middlewares in IO, because the RateLimiter needs mutable state.
processMiddlewareIO :: [MiddlewareConfig] -> IO Middleware
processMiddlewareIO cfgs = do
  mws <- mapM toMiddlewareIO cfgs
  pure (composeMiddleware mws)

-- Legacy/pure path; RateLimiter becomes a no-op here.
processMiddleware :: [MiddlewareConfig] -> Middleware
processMiddleware =
  composeMiddleware . map toMiddlewarePure
  where
    toMiddlewarePure (RateLimiter _) = id
    toMiddlewarePure x               = unsafeToMiddleware x

-- Compose like before: last in the list becomes the outermost wrapper.
composeMiddleware :: [Middleware] -> Middleware
composeMiddleware = foldl (flip (.)) id

unsafeToMiddleware :: MiddlewareConfig -> Middleware
unsafeToMiddleware AcceptOverride     = acceptOverride
unsafeToMiddleware Autohead           = autohead
unsafeToMiddleware Jsonp              = jsonp
unsafeToMiddleware MethodOverride     = methodOverride
unsafeToMiddleware MethodOverridePost = methodOverridePost
unsafeToMiddleware (Local s c)        = local (responseLBS (toEnum s) [] c)
unsafeToMiddleware (BasicAuth realm cred) =
  basicAuth (\u p -> pure $ (Just p ==) $ lookup u cred) (fromString realm)
unsafeToMiddleware (AddHeaders headers) = addHeaders headers
unsafeToMiddleware (RateLimiter _)      = id

-- IO-aware conversion (covers all cases, including RateLimiter)
toMiddlewareIO :: MiddlewareConfig -> IO Middleware
toMiddlewareIO AcceptOverride     = pure acceptOverride
toMiddlewareIO Autohead           = pure autohead
toMiddlewareIO Jsonp              = pure jsonp
toMiddlewareIO MethodOverride     = pure methodOverride
toMiddlewareIO MethodOverridePost = pure methodOverridePost
toMiddlewareIO (Local s c)        = pure $ local (responseLBS (toEnum s) [] c)
toMiddlewareIO (BasicAuth realm cred) =
  pure $ basicAuth (\u p -> pure $ (Just p ==) $ lookup u cred) (fromString realm)
toMiddlewareIO (AddHeaders headers) = pure $ addHeaders headers
-- Important: build Env once and return a pure Middleware, no per-request setup.
toMiddlewareIO (RateLimiter rl) = do
  env <- buildEnvFromConfig rl
  pure (buildRateLimiterWithEnv env)
