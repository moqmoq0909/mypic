{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.MinIO.S3Repository (multiUpload) where

-- fromMinioEnv,

import qualified Application.Global.GlobalState as GS
import qualified Application.Global.WSWorker as WSW
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Domain.Model.Book.BookEntity
  ( BookEntity (..),
  )
import Domain.Model.Image.ImageEntity
  ( ImageEntity (..),
  )
import Infrastructure.Repository.MinIO.S3Gateway (uploadObject)
import Network.Minio
  ( fromAWSEnv,
    setCreds,
  )
import qualified Network.WebSockets as WS
import Presentation.Router.CustomHandler (AppM)
import System.Environment (getEnv)
import Util ((|>))

-- mapConcurrently_関数でputObjectを非同期で複数同時実行
multiUpload :: [BookEntity] -> AppM [Either (BookEntity, [(ImageEntity, String)]) BookEntity]
multiUpload books = do
  gs <- ask

  liftIO $ do
    maybeCreds <- fromAWSEnv
    -- maybeCreds <- fromMinioEnv
    bucketName <- getEnv "MINIO_BUCKET_NAME"

    case maybeCreds of
      Just creds -> do
        -- 言語拡張OverloadedStringsにより、文字列をConnectInfo型として上書きしている。
        -- この方法でしかエンドポイントを設定できない
        let conn = setCreds creds "http://localhost:9000"

        workerResult <- GS.readWorker gs 1

        forM books $ \b -> do
          imgs <- forM (b |> images) $ \img -> do
            -- 責務としてはここで実行するので合ってる？
            let path = img |> filePath |> fromJust
            let url = img |> originalImgUrl

            res <- uploadObject conn (T.pack bucketName) (path, url)

            case res of
              Left e -> do
                putStrLn $ "「" ++ T.unpack path ++ "」のアップロードに失敗しました: " ++ show e
                return $ Left (img, show e)
              Right () -> do
                case workerResult of
                  Just (Just w) -> do
                    let wsconn = w |> WSW.conn
                    WS.sendTextData wsconn $ T.pack "1/10完了"
                    return $ Right img
                  Just Nothing -> do
                    putStrLn "既に接続が切れています"
                    return $ Right img
                  Nothing -> do
                    putStrLn "対応するWebsocket通信が存在しません"
                    return $ Right img

          -- アップロードに失敗した画像があるかチェック
          let failedUploads = lefts imgs

          if null failedUploads
            then do
              -- putStrLn $ "「" ++ show (bookId b) ++ "」のすべての画像のアップロードに成功しました。"
              return $ Right b -- この本のすべての画像のアップロードが成功
            else do
              -- putStrLn $ "「" ++ show (bookId b) ++ "」のアップロードに失敗した画像があります。"
              return $ Left (b, failedUploads)
      Nothing -> do
        -- 認証情報が取得できなかった場合の処理
        putStrLn "認証情報の取得に失敗しました。環境変数を確認してください。"
        return []

-- sum' :: [Int] -> Int
-- sum' xs = runST $ do
--   ref <- newSTRef 0
--   forM_ xs $ \i -> do
--     modifySTRef ref (+ i)
--   readSTRef ref

-- main :: IO ()
-- main = print $ sum' [1 .. 10]

-- 結果: 55