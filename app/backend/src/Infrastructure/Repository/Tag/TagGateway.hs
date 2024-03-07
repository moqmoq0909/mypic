{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Tag.TagGateway (findTagListByBooks) where

import Data.String (fromString)
import Database.MySQL.Simple (Connection, query)
import Database.MySQL.Simple.Types (In (..), Only (..), Query)
import Infrastructure.Connection (connectMySQL)
import qualified Infrastructure.Repository.Category.CategoryRecord as CaR
import qualified Infrastructure.Repository.Tag.TagRecord as TR
import Util ((|>))

-- タグ一覧を取得
findTagListByBooks :: [Int] -> IO [(String, CaR.CategoryRecord, TR.TagRecord)]
findTagListByBooks bookIds = do
  records <- connectMySQL . getTagList $ bookIds
  return $
    records
      |> map
        ( \(bookKey, categoryId, categoryKey, categoryName, categoryColor, tagKey, tagName) ->
            ( bookKey,
              CaR.CategoryRecord categoryKey categoryName categoryColor,
              TR.TagRecord tagKey tagName categoryId
            )
        )
  where
    getTagList :: [Int] -> Connection -> IO [(String, Int, String, String, String, String, String)]
    getTagList book_ids conn =
      query conn sqlQuery . Only . In $ book_ids
      where
        sqlQuery :: Query
        sqlQuery =
          fromString $
            "SELECT b.book_key, t.category_id, ca.category_key, ca.category_name, ca.category_color, t.tag_key, t.tag_name"
              ++ " FROM (SELECT * FROM books_tags WHERE book_id IN ? ) AS bt"
              ++ " INNER JOIN books b ON bt.book_id = b.id"
              ++ " INNER JOIN tags t ON bt.tag_id = t.id"
              ++ " INNER JOIN categories ca ON t.category_id = ca.id"
