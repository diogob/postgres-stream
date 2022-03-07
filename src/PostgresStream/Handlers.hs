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
import qualified Streamly.Internal.Data.Array.Stream.Fold.Foreign as Stramly
import qualified Streamly.Prelude as Streamly
import System.Log.FastLogger (pushLogStrLn, toLogStr)

file :: Text -> AppM (Streamly.SerialT IO ByteString)
file id = do
  pool <- asks getPool
  fileOrError <- DB.file pool id
  case fileOrError of
    Right file -> pure $ Streamly.fromPure file
    Left e -> err e
  where
    err :: ApiError -> AppM (Streamly.SerialT IO ByteString)
    err (Error msg) = throwError $ err503 {errBody = toSL msg}
