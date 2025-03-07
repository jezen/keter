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

import Data.ByteString as S (ByteString)
import Data.ByteString.Lazy as L (ByteString)

import Data.String (fromString)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)
import Keter.Aeson.KeyHelper qualified as AK (empty, toKey, toList, toText)

data MiddlewareConfig = AcceptOverride
                      | Autohead
                      | Jsonp
                      | MethodOverride
                      | MethodOverridePost
                      | AddHeaders ![(S.ByteString, S.ByteString)]
                      | BasicAuth !String ![(S.ByteString, S.ByteString)]
                         -- ^ Realm [(username,password)]
                      | Local !Int !L.ByteString
                         -- ^ Status Message
          deriving (Show,Generic)

instance FromJSON MiddlewareConfig where
  parseJSON (String "accept-override"     ) = pure AcceptOverride
  parseJSON (String "autohead"            ) = pure Autohead
  parseJSON (String "jsonp"               ) = pure Jsonp
  parseJSON (String "method-override"     ) = pure MethodOverride
  parseJSON (String "method-override-post") = pure MethodOverridePost
  parseJSON (Object o) =
     case AK.toList o of
      [("basic-auth", Object o')] -> BasicAuth  <$> o' .:? "realm" .!= "keter"
                                                <*> (map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8) . AK.toList <$> o' .:? "creds"   .!= AK.empty)
      [("headers"   , Object _ )]    -> AddHeaders . map ((T.encodeUtf8 . AK.toText) *** T.encodeUtf8) . AK.toList <$> o  .:? "headers" .!= AK.empty
      [("local"     , Object o')] -> Local  <$> o' .:? "status" .!=  401
                                            <*> (TL.encodeUtf8 <$> o' .:? "message" .!= "Unauthorized Accessing from Localhost ONLY" )
      _                      -> mzero -- fail "Rule: unexpected format"
  parseJSON _ = mzero

instance ToJSON MiddlewareConfig where
  toJSON AcceptOverride     = "accept-override"
  toJSON Autohead           = "autohead"
  toJSON Jsonp              = "jsonp"
  toJSON MethodOverride     = "method-override"
  toJSON MethodOverridePost = "method-override-post"
  toJSON (BasicAuth realm cred) = object [ "basic-auth" .= object [ "realm" .= realm
                                                                  , "creds" .= object ( map ( (AK.toKey . T.decodeUtf8) *** (String . T.decodeUtf8)) cred )
                                                                  ]
                                         ]
  toJSON (AddHeaders headers)   = object [ "headers"    .= object ( map ((AK.toKey . T.decodeUtf8) *** String . T.decodeUtf8) headers)  ]
  toJSON (Local sc msg)         = object [ "local"      .= object [ "status" .= sc
                                                                  , "message" .=  TL.decodeUtf8 msg
                                                                  ]
                                         ]


{-- Still missing
-- CleanPath
-- Gzip
-- RequestLogger
-- Rewrite
-- Vhost
--}

processMiddleware :: [MiddlewareConfig] -> Middleware
processMiddleware = composeMiddleware . map toMiddleware

toMiddleware :: MiddlewareConfig -> Middleware
toMiddleware AcceptOverride     = acceptOverride
toMiddleware Autohead           = autohead
toMiddleware Jsonp              = jsonp
toMiddleware (Local s c )       = local ( responseLBS (toEnum s) [] c )
toMiddleware MethodOverride     = methodOverride
toMiddleware MethodOverridePost = methodOverridePost
toMiddleware (BasicAuth realm cred) = basicAuth (\u p -> return $ (Just p ==) $ lookup u cred ) (fromString realm)
toMiddleware (AddHeaders headers)   = addHeaders headers

-- composeMiddleware :
composeMiddleware :: [Middleware] -> Middleware
composeMiddleware = foldl (flip (.)) id
