{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Repository.Image.ImageRecord (ImageRecord (..), fromJson) where

import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.String ()
import Data.Text (Text)
import GHC.Generics (Generic)

data ImageRecord = Image
  { imageId :: Int,
    imageKey :: String,
    imageName :: String,
    imageUrl :: Text,
    imageType :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON ImageRecord

instance ToJSON ImageRecord

-- JSON文字列から[ImageRecord]への変換
fromJson :: BL.ByteString -> [ImageRecord]
fromJson jsonString =
  fromMaybe [] (decode jsonString)
