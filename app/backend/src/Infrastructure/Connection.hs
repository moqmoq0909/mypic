{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Connection (connectMySQL) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Database.MySQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectPort,
        connectUser
      ),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Util ((|>))

-- クエリーを生成する関数を受け取りデータベースと接続する関数
connectMySQL :: (Connection -> IO a) -> IO a
connectMySQL action = do
  -- 環境変数から接続情報を取得
  host <- getEnv "MYSQL_HOST"
  portStr <- getEnv "MYSQL_PORT"
  user <- getEnv "MYSQL_USER"
  password <- getEnv "MYSQL_PASSWORD"
  database <- getEnv "TARGET_DATABASE"

  -- もし環境変数MYSQL_PORTが設定されていない、または数値に変換できない値であった場合、デフォルト値3306が使用される
  let port = readMaybe portStr |> fromMaybe 3306

  let connectInfo =
        defaultConnectInfo
          { connectHost = host,
            connectPort = port,
            connectUser = user,
            connectPassword = password,
            connectDatabase = database
          }

  action
    |> bracket
      (connect connectInfo)
      close
