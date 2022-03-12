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

file :: MonadIO m => PGPool -> Text -> m (Either ApiError ByteString)
file pool id = liftIO $ ResourcePool.withResource pool selectVersion
  where
    selectVersion con = do
      version <- runMaybeT $ selectOne con "SELECT version()"
      pure $ maybeToRight (Error "Error executing SQL") version

    selectOne con sql = do
      maybeResult <- MaybeT $ PG.exec con sql
      MaybeT $ PG.getvalue maybeResult 0 0