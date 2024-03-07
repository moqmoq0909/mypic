{-# LANGUAGE DeriveGeneric #-}

module Presentation.APIRes (APIRes (..)) where

import Data.Aeson (ToJSON)
import Data.String ()
import GHC.Generics (Generic)

-- data TweetUploadReq = TweetUploadReq
--   {twitterUserName :: String}
--   deriving (Eq, Show, Generic)

data APIRes = APIRes
  { status :: Int,
    message :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON APIRes
