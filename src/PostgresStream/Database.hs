module PostgresStream.Database
  ( file,
    -- re-exports
    Pool,
    UsageError,
    acquire,
    release,
    LocalTime,
  )
where

import qualified Data.Aeson as JSON
import Data.Either.Combinators (mapBoth, mapLeft)
import Data.Functor.Contravariant ((>$<))
import Data.Vector
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Hasql.Pool (Pool, UsageError, acquire, release, use)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import PostgreSQL.Binary.Data (LocalTime)
import PostgresStream.Domain
import PostgresStream.Prelude

file :: MonadIO m => Pool -> Text -> m (Either ApiError ByteString)
file pool id = liftIO mapError
  where
    mapError = mapLeft (\_ -> Error "Database Error") <$> use pool (statement id selectfile)
    selectfile :: Statement Text ByteString
    selectfile = Statement sql encoder decoder True
    sql = "SELECT version()"
    encoder = HE.param (HE.nonNullable HE.text)
    decoder = HD.singleRow (HD.column (HD.nonNullable HD.bytea))
