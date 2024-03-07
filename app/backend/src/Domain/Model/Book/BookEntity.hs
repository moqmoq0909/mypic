{-# LANGUAGE DeriveGeneric #-}

module Domain.Model.Book.BookEntity
  ( BookEntity (..),
    MediaType (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Domain.Model.Image.ImageEntity (ImageEntity (..))
import GHC.Generics (Generic)

data MediaType = Comic | Twitter deriving (Eq, Show, Generic)

instance ToJSON MediaType

data BookEntity = BookEntity
  { bookKey :: String,
    bookName :: Maybe String,
    bookText :: Maybe Text,
    hasRead :: Bool,
    mediaType :: MediaType,
    images :: [ImageEntity],
    creatorId :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON BookEntity