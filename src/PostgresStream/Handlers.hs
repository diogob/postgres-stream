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
import Servant.Types.SourceT (source)
import System.Log.FastLogger (pushLogStrLn, toLogStr)

file :: Text -> AppM (SourceIO ByteString)
file id = do
  pool <- asks getPool
  fileOrError <- DB.file pool id
  case fileOrError of
    Right file -> pure $ source [file]
    Left e -> err e
  where
    err :: ApiError -> AppM (SourceIO ByteString)
    err (Error msg) = throwError $ err503 {errBody = toSL msg}
