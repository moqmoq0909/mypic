-- 作者ごとにbookをまとめる、アップロードした日付ごとにbookをまとめる、1,2,3軍でbookをまとめる、用途ごとにまとめる(顔手本、体手本、動物手本)など
{-# LANGUAGE DeriveGeneric #-}

module Domain.Model.Table.Table
  ( TableReq (TReq, GTReq),
    TableRes (TRes),
  )
where

import Data.Aeson (ToJSON)
import Data.String ()
import Domain.Model.Table.CompositeRow (Row)
import GHC.Generics (Generic)
import Infrastructure.Repository.Category.CategoryRecord (CategoryRecord)
import Infrastructure.Repository.Tag.TagRecord (TagRecord)

data TableReq
  = TReq Int
  | GTReq Int
  deriving (Eq, Show, Generic)

data TableRes = TRes
  { creators :: [Row],
    books :: [(String, [Row])],
    tags :: [(String, [(CategoryRecord, [TagRecord])])]
  }
  deriving (Eq, Show, Generic)

instance ToJSON TableRes