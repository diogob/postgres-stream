module PostgresStream.Database
  ( file,
    acquire,
    PGPool,
    -- re-exports
    Pool,
    UsageError,
    release,
    LocalTime,
  )
where

import qualified Data.Aeson as JSON
import Data.Either.Combinators (mapBoth, mapLeft)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Pool as ResourcePool
import Data.Time
import Data.Vector
import qualified Database.PostgreSQL.LibPQ as PG
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Hasql.Pool (Pool, UsageError, release, use)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
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
      maybeResult <- PG.exec con sql
      case maybeResult of
        Nothing -> pure $ Left $ Error "Some error executing SQL"
        Just r -> do
          maybeToRight (Error "Some error fetching SQL") <$> PG.getvalue r 0 0
    sql = "SELECT version()"
