{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Repository.Creator.CreatorRecord
  ( CreatorRecord (..),
  )
where

import Data.Aeson (ToJSON)
import Data.String ()
import GHC.Generics (Generic)

data CreatorRecord = CreatorRecord
  { creatorKey :: String,
    creatorName :: String
    -- profile :: Profile
  }
  deriving (Eq, Show, Generic)

instance ToJSON CreatorRecord