{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Repository.MinIO.S3Gateway (uploadObject) where

import Data.Conduit (yield)
import Data.Text (Text, unpack)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Network.Minio
  ( ConnectInfo,
    MinioErr,
    defaultPutObjectOptions,
    putObject,
    runMinio,
  )

-- minio-hsのputObjectの引数はText型でなければならない
uploadObject :: ConnectInfo -> Text -> (Text, Text) -> IO (Either MinioErr ())
uploadObject conn bucketName (filePath, url) = do
  -- s3はファイル名に階層を含めることで疑似的にディレクトリを作り出す
  let objectName = filePath

  -- 画像や動画の取得
  request <- parseRequest $ unpack url
  response <- httpBS request

  -- TODO urlが無効な場合のエラーハンドリングが必要
  let imageData = getResponseBody response
  let source = yield imageData

  runMinio conn $ putObject bucketName objectName source Nothing defaultPutObjectOptions
