{-# LANGUAGE ScopedTypeVariables #-}

module PostgresStream.Handlers
  ( file,
  )
where

import Data.Aeson (toJSON)
import Data.Vector
import PostgresStream.AppM
import qualified PostgresStream.Database as DB
import PostgresStream.Domain
import PostgresStream.Prelude
import Servant
import Servant.Types.SourceT (SourceT, fromAction, source)
import System.Log.FastLogger (pushLogStrLn, toLogStr)

file :: Text -> AppM (SourceIO ByteString)
file id = do
  pool <- asks getPool
  pure $
    fromAction (const False) $ do
      fileOrError <- DB.file pool id
      threadDelay 1000000
      case fileOrError of
        Right file -> pure file
        Left e -> throwIO e