{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module PostgresStream.Domain
  ( File (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Vector hiding (drop)
import PostgresStream.Prelude

newtype File = File
  { unfile :: Text
  }
  deriving (Generic, Typeable)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''File)
