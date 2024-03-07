{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Creator.CreatorGateway (findCreatorList) where

import Data.String (fromString)
import Database.MySQL.Simple (Connection, query)
import Database.MySQL.Simple.Types (In (..), Only (..), Query)
import Infrastructure.Connection (connectMySQL)
import qualified Infrastructure.Repository.Creator.CreatorRecord as CrR
import Util ((|>))

-- 著者一覧を取得
findCreatorList :: [Int] -> IO [CrR.CreatorRecord]
findCreatorList creatorIds = do
  records <- connectMySQL $ getCreatorList creatorIds
  return $ records |> map (uncurry CrR.CreatorRecord)
  where
    -- uncurry関数は、2つの引数を取る関数を、1つの引数（この引数は2つの要素を持つタプル）を取る関数に変換する。
    -- return $ records |> map (\(creatorKey, creatorName) -> CrR.CreatorRecord creatorKey creatorName)

    getCreatorList :: [Int] -> Connection -> IO [(String, String)]
    getCreatorList creator_ids conn =
      query conn sqlQuery $ Only $ In creator_ids
      where
        sqlQuery :: Query
        sqlQuery =
          fromString $
            "SELECT creator_key, creator_name"
              ++ " FROM creators WHERE id IN ?"