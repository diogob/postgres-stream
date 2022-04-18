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
  let loop offset = Effect $ step offset <$> readChunk offset
      step offset x
        | BS.length x == 0 = Stop
        | otherwise = Yield x (loop (offset + chunkSize))

      readChunk :: Int -> IO ByteString
      readChunk offset = do
        fileOrError <- DB.file pool id chunkSize offset
        case fileOrError of
          Right file -> pure file
          Left e -> throwIO e

  pure $ fromStepT $ loop 0

-- do
-- -- fileOrError <- DB.file pool id
-- tid <- myThreadId
-- liftIO $ print ("Received " <> id <> " in thread " <> show tid)
-- threadDelay 100
-- pure ("Hi there from " <> show tid)

-- case fileOrError of
--   Right file -> pure file
--   Left e -> throwIO e