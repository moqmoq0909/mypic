{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Model.Image.ImageEntity
  ( ImageEntity (..),
    toJson,
    extractFileName,
    ImageType (..),
    checkImageType,
  )
where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.String ()
import qualified Data.Text as T
import GHC.Generics (Generic)

-- TODO アップロードドメイン(保管部門)、ダウンロードドメイン(取り出し部門)というフォルダを作成し、その中でそれぞれアップロード前のImageEntityとアップロード後のImageEntityを作成する
-- ★直和型(複数の値コンストラクタを持つデータ型)の場合、特定の値コンストラクタにしか存在しないフィールドがある場合、Maybe型を返すgetter関数を定義する必要があり境界づけられたコンテクストを横断してしまうので、あえて使わない。

data ImageType = Photo | Video deriving (Eq, Show, Generic)

instance ToJSON ImageType

-- TODO data型をインターフェース、その型を使った関数を実装部分として分ける
checkImageType :: String -> ImageType
checkImageType str = case str of
  "photo" -> Photo
  "video" -> Video
  _ -> error "Unsupported media type"

data ImageEntity = ImageEntity
  { imageId :: Int,
    imageKey :: String,
    imageName :: String,
    originalImgUrl :: T.Text, -- urlからファイル名を抜き出す処理があるのでStringよりTextの方が操作しやすい
    filePath :: Maybe T.Text, -- minioのファイルパス
    imageType :: ImageType
  }
  deriving (Eq, Show, Generic)

instance ToJSON ImageEntity

-- ImageEntity 型を JSON 形式の String に変換する関数
-- ※Aesonのencode関数はToJSON型クラスのインスタンスに対して実行できる
toJson :: [ImageEntity] -> String
toJson images = BLC.unpack (encode images)

-- URLの末尾からファイル名を抽出し、クエリパラメータを除去する関数
extractFileName :: T.Text -> String
extractFileName url =
  let fileNameWithQuery = case T.splitOn "/" url of
        [] -> "unknown"
        parts -> last parts
      -- '?'がある場合、それ以前の文字列を取得
      fileName = takeWhile (/= '?') (T.unpack fileNameWithQuery)
   in fileName -- クエリパラメータを除去したファイル名を返す