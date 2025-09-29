{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "=== Backend starting ==="
  mp <- lookupEnv "PORT"
  let p = maybe 8081 id (mp >>= readMaybe)
  hPutStrLn stderr $ "Backend will listen on port " ++ show p
  hFlush stderr
  run p app

app :: Application
app _ respond =
  respond $ responseLBS status200 [("Content-Type","text/plain")] "ok from rate-limited backend"
