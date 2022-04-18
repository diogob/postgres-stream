{-# LANGUAGE ScopedTypeVariables #-}

module PostgresStream.Handlers
  ( file,
  )
where

import Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import PostgresStream.AppM
import qualified PostgresStream.Database as DB
import PostgresStream.Domain
import PostgresStream.Prelude
import Servant
import Servant.Types.SourceT (SourceT, StepT (..), fromAction, fromStepT, source)
import System.Log.FastLogger (pushLogStrLn, toLogStr)

chunkSize :: Int
chunkSize = 1000

file :: Text -> AppM (SourceIO ByteString)
file id = do
  pool <- asks getPool
  pure $ fromStepT $ loop pool 0
  where
    loop pool offset = Effect $ step pool offset <$> readChunk pool offset
    step pool offset x
      | BS.length x /= chunkSize = Stop
      | otherwise = Yield x (loop pool (offset + chunkSize))

    readChunk :: DB.PGPool -> Int -> IO ByteString
    readChunk pool offset = do
      fileOrError <- DB.file pool id chunkSize offset
      case fileOrError of
        Right file -> pure file
        Left e -> throwIO e

-- do
-- -- fileOrError <- DB.file pool id
-- tid <- myThreadId
-- liftIO $ print ("Received " <> id <> " in thread " <> show tid)
-- threadDelay 100
-- pure ("Hi there from " <> show tid)

-- case fileOrError of
--   Right file -> pure file
--   Left e -> throwIO e