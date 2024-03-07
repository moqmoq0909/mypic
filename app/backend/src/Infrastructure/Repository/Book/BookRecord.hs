{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Repository.Book.BookRecord
  ( BookRecord (..),
    printBooks,
  )
where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.String ()
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Infrastructure.Repository.Image.ImageRecord (ImageRecord (..))

data BookRecord = BookRecord
  { bookId :: Int,
    bookKey :: String,
    bookName :: Maybe String,
    bookText :: Maybe Text,
    hasRead :: Bool,
    mediaType :: String,
    images :: [ImageRecord],
    creatorId :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON BookRecord

-- BookRecordのリストをJSON文字列に変換してコンソールに出力
printBooks :: [BookRecord] -> IO ()
printBooks books = do
  putStrLn "==== [BookRecord]を出力 ===="
  BLC.putStrLn $ encode books