{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering), putStrLn)

main :: IO ()
main = do
  -- Flush each line immediately – this prevents our log lines from mixing
  -- with the HTTP payload that the test harness reads.
  hSetBuffering stdout LineBuffering

  putStrLn "=== Backend starting ==="
  mp <- lookupEnv "PORT"
  let p = maybe 8081 id (mp >>= readMaybe)
  putStrLn $ "Backend will listen on port " ++ show p
  run p app

app :: Application
app _ respond =
  respond $ responseLBS status200 [("Content-Type","text/plain")]
            "ok from rate‑limited backend"
