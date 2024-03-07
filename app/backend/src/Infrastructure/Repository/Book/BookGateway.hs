{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Book.BookGateway (findBookList, insertBooks) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.MySQL.Simple (Connection, execute, query)
import Database.MySQL.Simple.Types (Only (..), Query)
import qualified Domain.Model.Book.BookEntity as BE
import qualified Domain.Model.Image.ImageEntity as IE
import Infrastructure.Connection (connectMySQL)
import qualified Infrastructure.Repository.Book.BookRecord as BR
import qualified Infrastructure.Repository.Image.ImageRecord as IR
import Util ((|>))

-- フック一覧を取得
findBookList :: Int -> IO [BR.BookRecord]
findBookList tagId = do
  records <- connectMySQL $ getBookList tagId
  putStrLn "==== imagesのJSONです ===="
  records
    |> mapM
      ( \(bookId, key, name, text, hasRead, mediaType, imagesJSON, creatorId, createdAt, updatedAt) -> do
          _ <- BLC.putStrLn imagesJSON
          return $ BR.BookRecord bookId key name text hasRead mediaType (imagesJSON |> IR.fromJson) creatorId createdAt updatedAt
      )
  where
    getBookList :: Int -> Connection -> IO [(Int, String, Maybe String, Maybe T.Text, Bool, String, BL.ByteString, Int, UTCTime, UTCTime)]
    getBookList tag_id conn =
      query conn sqlQuery $ Only tag_id
      where
        sqlQuery :: Query
        sqlQuery =
          fromString $
            "SELECT b.id, b.book_key, b.book_name, b.book_text, b.has_read, b.media_type, b.images, b.creator_id, b.created_at, b.updated_at"
              ++ " FROM (SELECT * FROM tags WHERE id = ? ) AS filtered_tags"
              ++ " INNER JOIN books_tags bt ON filtered_tags.id = bt.tag_id"
              ++ " INNER JOIN books b ON bt.book_id = b.id"

{-
  query conn sqlQuery $ Only tag_id は
  query conn sqlQuery (Only tag_id) と同義
  ※ 「$」を使用すると、その右側の表現が先に評価される
-}

{-
  「++」を使うことで[Char]型と認識されるので、
  fromStringで明示的に文字列をQuery型に変換する
  ※ 言語拡張「LANGUAGE OverloadedStrings」がないと自動的に変換されない
  [Char] -> String -> Query
-}

-- 複数レコードを一度に挿入するクエリ
insertBooks :: [BE.BookEntity] -> IO ()
insertBooks books = do
  res <- connectMySQL (multipleInsert books)
  case res of
    -- 1件でも失敗すると挿入が取消(ロールバック)される
    0 -> putStrLn "insert失敗"
    _ -> putStrLn "insert成功"

multipleInsert :: [BE.BookEntity] -> Connection -> IO Int
multipleInsert bookEntities conn = do
  let sqlQuery =
        "INSERT INTO books (book_key, book_name, has_read, media_type, images, creator_id) VALUES "
          ++ intercalate ", " (replicate (length bookEntities) "(?, ?, ?, ?, ?, ?)")
  rowCount <- execute conn (fromString sqlQuery) (toQueryParams bookEntities)
  return (fromIntegral rowCount)

-- ★mysql-simpleのParam型クラスのインスタンスには、多くの標準 Haskell 型が含まれているため、通常は明示的な変換関数を呼び出す必要はありません。execute や query などの関数は、自動的に Param 型クラスのインスタンスを SqlValue に変換します。

-- concatMapはJSのflatMapと同じ、mapをしてさらに平坦化してくれる関数
toQueryParams :: [BE.BookEntity] -> [String]
toQueryParams bookEntities = bookEntities |> concatMap toStrings
  where
    toStrings :: BE.BookEntity -> [String]
    toStrings be =
      [ be |> BE.bookKey,
        be |> BE.bookName |> fromMaybe "",
        be |> BE.hasRead |> show,
        be |> BE.mediaType |> show,
        be |> BE.images |> IE.toJson,
        be |> BE.creatorId |> show
      ]