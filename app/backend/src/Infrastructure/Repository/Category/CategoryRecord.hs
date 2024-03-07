{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Repository.Category.CategoryRecord (CategoryRecord (..)) where

import Data.Aeson (ToJSON)
import Data.String ()
import GHC.Generics (Generic)

data CategoryRecord = CategoryRecord
  { categoryKey :: String,
    categoryName :: String,
    categoryColor :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON CategoryRecord