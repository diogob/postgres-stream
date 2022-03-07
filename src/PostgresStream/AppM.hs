{-# LANGUAGE DeriveGeneric #-}

module PostgresStream.AppM where

import Control.AutoUpdate
  ( defaultUpdateSettings,
    mkAutoUpdate,
    updateAction,
  )
import Control.Monad.Time
import Data.Aeson (FromJSON, ToJSON (..), encode, genericToEncoding)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import PostgresStream.Database
import PostgresStream.Prelude
import Servant
import System.Log.FastLogger
  ( LoggerSet,
    ToLogStr (..),
    defaultBufSize,
    flushLogStr,
    newStdoutLoggerSet,
    pushLogStrLn,
  )

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx
  { config :: Config,
    getLogger :: LoggerSet,
    getPool :: Pool,
    getTime :: IO UTCTime
  }

data Config = Config
  { db :: Text,
    port :: Integer
  }
  deriving (Show)

data LogMessage = LogMessage
  { ltime :: !UTCTime,
    lmessage :: !Text,
    level :: !Text,
    lversion :: !Text,
    lenvironment :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

mkLogger :: IO LoggerSet
mkLogger = newStdoutLoggerSet defaultBufSize

mkGetTime :: IO (IO UTCTime)
mkGetTime = mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}

pushLogEntry :: Text -> AppM ()
pushLogEntry msg = do
  logset <- asks getLogger
  getTime <- asks getTime
  time <- liftIO getTime

  let logMsg =
        LogMessage
          { ltime = time,
            lmessage = msg,
            level = "info",
            lversion = "1.1.1",
            lenvironment = "development"
          }
  liftIO $ pushLogStrLn logset $ toLogStr logMsg
