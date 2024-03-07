{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Domain.Model.Table.CompositeRow
  ( Row (..),
    CommonRecord (..),
    toGroupMapTupple,
    toRows,
  )
where

import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Infrastructure.Repository.Book.BookRecord (BookRecord (..))
import Infrastructure.Repository.Creator.CreatorRecord (CreatorRecord (..))
import Util ((|>))

-- コンポジションパターン用の型
data Row = Row
  { id :: String, -- idにはcodeを入れる、フロントエンドではidと呼称しているため
    name :: String,
    uploadDate :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Row

-- creator,book,imageをコンポジションパターン用のRow型に変換する処理
-- 型クラスはTypeScriptの型制約に近い
class CommonRecord a where
  getKey :: a -> String
  getName :: a -> String
  getBelongId :: a -> Int
  getDate :: a -> Maybe UTCTime

instance CommonRecord BookRecord where
  getKey :: BookRecord -> String
  getName :: BookRecord -> String
  getBelongId :: BookRecord -> Int
  getDate :: BookRecord -> Maybe UTCTime
  getKey = bookKey
  getName br = br |> bookName |> fromMaybe ""
  getBelongId = creatorId
  getDate = Just . updatedAt

instance CommonRecord CreatorRecord where
  getKey :: CreatorRecord -> String
  getName :: CreatorRecord -> String
  getBelongId :: CreatorRecord -> Int
  getDate :: CreatorRecord -> Maybe UTCTime
  getKey = creatorKey
  getName = creatorName
  getBelongId _ = 0
  getDate = const Nothing

-- コンポジションパターン用のutil関数
toGroupMapTupple :: (CommonRecord a) => String -> [[a]] -> [(String, [Row])]
toGroupMapTupple prefix groupedRecords =
  groupedRecords |> map (toMapTupple prefix)

toMapTupple :: (CommonRecord a) => String -> [a] -> (String, [Row])
toMapTupple prefix records@(rHead : _) =
  let code = prefix ++ show (getBelongId rHead)
      rows = map toRows records
   in (code, rows)
toMapTupple _ [] = error "Empty group should not exist"

toRows :: (CommonRecord a) => a -> Row
toRows record =
  Row (getKey record) (getName record) (getDate record)