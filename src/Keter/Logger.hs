{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Keter.Logger
    ( Logger(..)
    , createLoggerViaConfig
    , defaultRotationSpec
    , defaultMaxTotal
    , defaultBufferSize
    ) where

import Data.Time
import Debug.Trace
import qualified System.Log.FastLogger as FL
import System.Directory
import System.FilePath
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Keter.Context
import Keter.Config.V10

-- | Record wrapper over a fast logger (log,close) function tuple, just to make it less unwieldy and obscure.
-- The 'LogType' is also tracked, in case formatting depends on it.
data Logger = Logger
  { loggerLog :: forall a. FL.ToLogStr a => a -> IO ()
  , loggerClose :: IO () 
  , loggerType :: FL.LogType
  }

-- | Create a logger based on a 'KeterConfig'.
-- If log rotation is enabled in the config, this will return a rotating file logger;
-- and a stderr logger otherwise.
createLoggerViaConfig :: KeterConfig
                      -> String -- ^ Log file name
                      -> IO Logger
createLoggerViaConfig KeterConfig{..} name = do
  let logFile = kconfigDir </> "log" </> name <.> "log"
  let logType =
       if kconfigRotateLogs
         then FL.LogFile (defaultRotationSpec logFile) defaultBufferSize
         else FL.LogStderr defaultBufferSize
  liftIO $ createDirectoryIfMissing True (takeDirectory logFile)
  mkLogger logType <$> FL.newFastLogger logType
  where
    mkLogger logType (logFn, closeFn) = Logger (logFn . FL.toLogStr) closeFn logType

defaultRotationSpec :: FilePath -> FL.FileLogSpec
defaultRotationSpec dir =
    FL.FileLogSpec dir defaultMaxTotal maxBound -- TODO: do we want to overwrite logs after a certain point? leaving this INT_MAX for now

-- | The default total file size before for a log file before it needs to be rotated
defaultMaxTotal :: Integer
defaultMaxTotal = 5 * 1024 * 1024 -- 5 MB

-- | The default log message buffer size
defaultBufferSize :: Int
defaultBufferSize = 256 -- 256 bytes, TODO: Reasonable value?
