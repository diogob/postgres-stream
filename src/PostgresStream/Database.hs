module PostgresStream.Database
  ( file,
    acquire,
    PGPool,
    -- re-exports
    LocalTime,
  )
where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Aeson as JSON
import Data.Either.Combinators (mapBoth, mapLeft)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Pool as ResourcePool
import Data.Time
import Data.Vector
import qualified Database.PostgreSQL.LibPQ as PG
import Foreign.C.Types (CUInt (..))
import GHC.IO.Device (SeekMode (..))
import PostgreSQL.Binary.Data (LocalTime)
import PostgresStream.Domain
import PostgresStream.Prelude

type PGPool = ResourcePool.Pool PG.Connection

type Settings =
  (Int, NominalDiffTime, ByteString)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO PGPool
acquire (size, timeout, connectionSettings) =
  ResourcePool.createPool acquire release stripes timeout size
  where
    acquire =
      PG.connectdb connectionSettings
    release =
      PG.finish
    stripes =
      1

file :: MonadIO m => PGPool -> Text -> Int -> Int -> m (Either ApiError ByteString)
file pool id limit offset = liftIO $ ResourcePool.withResource pool selectVersion
  where
    selectVersion con = do
      file <- runMaybeT $ selectBytes con (toSL id)
      pure $ maybeToRight (Error "Error executing SQL") file

    selectBytes con id = do
      escapedId <- MaybeT $ PG.escapeStringConn con id
      void $ MaybeT $ PG.exec con "BEGIN"
      maybeResult <- MaybeT $ PG.exec con ("SELECT content FROM files WHERE name = '" <> escapedId <> "'")
      oidBytes <- MaybeT $ PG.getvalue' maybeResult 0 0
      oid <- MaybeT $ pure $ PG.Oid <$> readMaybe oidBytes
      fd <- MaybeT $ PG.loOpen con oid ReadMode
      MaybeT $ PG.loSeek con fd AbsoluteSeek offset
      contentBytes <- MaybeT $ PG.loRead con fd limit
      void $ MaybeT $ PG.exec con "COMMIT"
      pure contentBytes
