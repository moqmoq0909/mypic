{-# LANGUAGE FlexibleContexts #-}

-- {-# LANGUAGE OverloadedRecordDot #-}

{-
  record.bookCodeのような形で、
  プロパティにアクセスできるようにする言語拡張
-}

module Domain.Model.Table.TableRepository
  ( makeTable,
  )
where

import Control.Monad.IO.Class (liftIO)
-- liftIOを使うことでIO型をHandlerに上げることができる

-- -- tagをbookCodeとcategoryでグループ化する処理用import

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Domain.Model.Table.CompositeRow (toGroupMapTupple, toRows)
import Domain.Model.Table.Table (TableRes (TRes))
import Infrastructure.Repository.Book.BookGateway (findBookList)
import qualified Infrastructure.Repository.Book.BookRecord as BR
import Infrastructure.Repository.Category.CategoryRecord (CategoryRecord)
import Infrastructure.Repository.Creator.CreatorGateway (findCreatorList)
import Infrastructure.Repository.Tag.TagGateway (findTagListByBooks)
import Infrastructure.Repository.Tag.TagRecord (TagRecord)
import Presentation.Router.CustomHandler (AppM)
import Servant (Handler)
import Util ((|>))

makeTable :: Int -> AppM TableRes
makeTable tagId = do
  books <-
    tagId
      |> findBookList
      |> liftIO

  -- ログ出力
  _ <-
    books
      |> BR.printBooks
      |> liftIO

  let groupedBooks = books |> toGroupBooksByCreator |> toGroupMapTupple "creator_"

  creators <-
    books
      |> map BR.creatorId -- booksからcreatorIdの配列を取得
      |> findCreatorList -- creatorIdの配列をもとにCreatorの配列を取得
      |> liftIO -- IOモナドをHandlerモナドに持ち上げる
  let creatorRows = creators |> map toRows

  tags <-
    books
      |> map BR.bookId
      |> findTagListByBooks
      |> liftIO

  let groupedTags = tags |> toGroupTags

  return $ TRes creatorRows groupedBooks groupedTags

-- creatorIdごとにBookをグループ分けする関数
toGroupBooksByCreator :: [BR.BookRecord] -> [[BR.BookRecord]]
toGroupBooksByCreator books =
  books
    |> sortBy (comparing BR.creatorId)
    |> groupBy (\x y -> BR.creatorId x == BR.creatorId y)

-- tagをbookKeyとcategoryでグループ分けする処理
toGroupTags :: [(String, CategoryRecord, TagRecord)] -> [(String, [(CategoryRecord, [TagRecord])])]
toGroupTags records =
  records
    |> sortBy (compare `on` (\(bookKey, _, _) -> bookKey))
    |> groupBy ((==) `on` (\(bookKey, _, _) -> bookKey)) -- tagをbookKeyでグループ分け
    |> map groupByCategory -- グループ分けした各要素を、さらにcategoryでグループ分け
  where
    groupByCategory :: [(String, CategoryRecord, TagRecord)] -> (String, [(CategoryRecord, [TagRecord])])
    groupByCategory rs =
      case rs of
        [] -> error "Empty group - this should not happen with non-empty input records"
        ((bookKey, _, _) : _) ->
          ( bookKey,
            rs
              |> groupBy ((==) `on` (\(_, category, _) -> category))
              |> map gatherTags
          )

    -- 同じCategoryのtagを配列としてまとめる
    gatherTags :: [(String, CategoryRecord, TagRecord)] -> (CategoryRecord, [TagRecord])
    gatherTags rs =
      case rs of
        [] -> error "Empty group - this should not happen with non-empty input records"
        ((_, categoryRecord, _) : _) ->
          ( categoryRecord,
            rs |> map (\(_, _, tag) -> tag)
          )

{-
  TODO
  NonEmptyを使って、もっとバグが入りにくい実装を目指す
  https://eh-career.com/engineerhub/entry/2017/10/03/110000#%E8%AA%B2%E9%A1%8C2-NonEmpty%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%A6%E3%82%82%E3%81%A3%E3%81%A8%E3%83%90%E3%82%B0%E3%81%8C%E5%85%A5%E3%82%8A%E3%81%AB%E3%81%8F%E3%81%84%E5%AE%9F%E8%A3%85%E3%82%92%E7%9B%AE%E6%8C%87%E3%81%99
-}
