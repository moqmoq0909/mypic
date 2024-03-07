{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Category.CategoryGateway (findCategoryList) where

import Data.String ()
import Database.MySQL.Simple (Connection, query_)
import Infrastructure.Connection (connectMySQL)
import qualified Infrastructure.Repository.Category.CategoryRecord as CaR
import Util (uncurry3, (|>))

-- タグ一覧を取得
findCategoryList :: IO [CaR.CategoryRecord]
findCategoryList = do
  records <- connectMySQL getCategoryList
  return $ records |> map (uncurry3 CaR.CategoryRecord)
  where
    getCategoryList :: Connection -> IO [(String, String, String)]
    getCategoryList conn = query_ conn "SELECT category_key, category_name, category_color FROM categories"

-- category_idは無所属の時は、無所属という親に所属させる？