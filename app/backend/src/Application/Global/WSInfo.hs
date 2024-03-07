{-# LANGUAGE DeriveGeneric #-}

module Application.Global.WSInfo (WSInfo (..)) where

import qualified Application.Global.WSWorker as WSW
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data WSInfo = WSInfo
  { id :: WSW.Id,
    working :: Bool
  }
  deriving (Generic, Show, Eq)

instance ToJSON WSInfo

instance FromJSON WSInfo