module PostgresStream.Prelude
  ( module Exports,
    ApiError (..),
  )
where

import Protolude as Exports hiding (Handler)
import Protolude.Conv as Exports (toSL)

newtype ApiError = Error Text deriving (Eq, Show)