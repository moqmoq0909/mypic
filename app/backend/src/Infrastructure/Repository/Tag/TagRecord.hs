{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Repository.Tag.TagRecord (TagRecord (..)) where

import Data.Aeson (ToJSON)
import Data.String ()
import GHC.Generics (Generic)

data TagRecord = TagRecord
  { tagKey :: String,
    tagName :: String,
    categoryId :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON TagRecord