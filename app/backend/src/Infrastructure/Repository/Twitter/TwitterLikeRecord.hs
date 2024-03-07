{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Repository.Twitter.TwitterLikeRecord (TwitterResponse (..), Tweet (..), Media (..), Variant (..), PublicMetrics (..), Attachments (..), Meta (..), Includes (..), toBookEntities) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, Value)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Domain.Model.Book.BookEntity
  ( BookEntity (..),
    MediaType (..),
  )
import Domain.Model.Image.ImageEntity
  ( ImageEntity (..),
    checkImageType,
  )
import GHC.Generics (Generic)
import Util ((|>))

-- Twitterのレスポンス全体を表す型
data TwitterResponse = TwitterResponse
  { datas :: [Tweet], -- フィールド名"data"はHaskellの予約語なので"datas"としています
    includes :: Includes,
    meta :: Meta
  }
  deriving (Show, Generic)

-- dataが予約語なのでdatasとし、明示的にパース処理を定義
instance FromJSON TwitterResponse where
  parseJSON :: Value -> Parser TwitterResponse
  parseJSON = withObject "TwitterResponse" $ \v -> do
    datas <- v .: "data"
    includes <- v .: "includes"
    meta <- v .: "meta"
    return TwitterResponse {..}

-- 各ツイートのデータを表す型
data Tweet = Tweet
  { edit_history_tweet_ids :: [String],
    attachments :: Maybe Attachments,
    tweet_id :: String,
    text :: Text
  }
  deriving (Show, Generic)

-- idが予約語なのでtweet_idとし、明示的にパース処理を定義
instance FromJSON Tweet where
  parseJSON :: Value -> Parser Tweet
  parseJSON = withObject "Tweet" $ \v -> do
    edit_history_tweet_ids <- v .: "edit_history_tweet_ids"
    attachments <- v .:? "attachments"
    tweet_id <- v .: "id"
    text <- v .: "text"
    return Tweet {..}

-- メディアの情報を含む型
data Media = Media
  { image_url :: Maybe String,
    media_key :: String,
    image_type :: String, -- [注意]photo or video
    duration_ms :: Maybe Int,
    preview_image_url :: Maybe String,
    variants :: Maybe [Variant],
    public_metrics :: Maybe PublicMetrics
  }
  deriving (Show, Generic)

-- typeが予約語なのでimage_typeとし、明示的にパース処理を定義
instance FromJSON Media where
  parseJSON :: Value -> Parser Media
  parseJSON = withObject "Media" $ \v -> do
    image_url <- v .:? "url"
    media_key <- v .: "media_key"
    image_type <- v .: "type"
    duration_ms <- v .:? "duration_ms"
    preview_image_url <- v .:? "preview_image_url"
    variants <- v .:? "variants"
    public_metrics <- v .:? "public_metrics"
    return Media {..}

-- メディアのバリエーションを表す型
data Variant = Variant
  { bit_rate :: Maybe Int,
    content_type :: String,
    url :: String
  }
  deriving (Show, Generic)

instance FromJSON Variant

-- 公開メトリクスを表す型
newtype PublicMetrics = PublicMetrics
  { view_count :: Int
  }
  deriving (Show, Generic)

instance FromJSON PublicMetrics

-- ツイートに添付されたメディアの情報を含む型
newtype Attachments = Attachments
  { media_keys :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Attachments

-- レスポンスのメタデータを表す型
data Meta = Meta
  { result_count :: Int,
    next_token :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Meta

-- メディア全体を含む型
newtype Includes = Includes
  { media :: [Media]
  }
  deriving (Show, Generic)

instance FromJSON Includes

-- TwitterResponseを[BookEntity]に変換する処理
toBookEntities :: TwitterResponse -> [BookEntity]
toBookEntities twitterResponse =
  twitterResponse |> datas |> toBooks
  where
    -- 既存のTweetから新しいBookEntityを作成する補助関数
    toBooks :: [Tweet] -> [BookEntity]
    toBooks ts =
      [ BookEntity
          { bookKey = t |> tweet_id,
            bookName = Nothing,
            bookText = Just $ pack "ツイート内容",
            hasRead = False,
            mediaType = Twitter, -- TwitterAPIとのやりとりなのでTwitter
            images = t |> findMedias |> toImages,
            creatorId = 0
          }
        | t <- ts
      ]

    -- 特定のTweetから関連するMedia(画像や動画)のリストを抽出する関数
    findMedias :: Tweet -> [Media]
    findMedias tweet =
      case tweet |> attachments of
        Just atts ->
          atts
            |> media_keys
            |> map
              ( \mKey ->
                  twitterResponse
                    |> includes
                    |> media
                    |> filter
                      (\m -> media_key m == mKey)
                    |> head
              )
        Nothing -> []

    -- Media型をImageEntity型へ変換する関数
    toImages :: [Media] -> [ImageEntity]
    toImages ms =
      [ ImageEntity
          { imageId = i, -- TODO いらないかも
            imageKey = m |> media_key, -- TODO ユニークでなければならない
            imageName = twoDigits i,
            originalImgUrl = case m |> image_url of
              Just u -> pack u -- photoの場合
              Nothing -> getVideoUrl (m |> variants), -- videoの場合
            filePath = Nothing,
            imageType = m |> image_type |> checkImageType
          }
        | (m, i) <- zip ms [1 ..]
      ]

    -- videoの場合 最後から2番目の要素が一番解像度が高い
    -- TODO jpg,png,mp4以外を除外する処理を入れる
    -- TODO Nothingの場合のエラーハンドリングができていない
    getVideoUrl :: Maybe [Variant] -> Text
    getVideoUrl =
      maybe
        "No URL"
        ( \vs ->
            vs
              |> map url
              |> reverse
              |> drop 1
              |> listToMaybe
              |> maybe
                "No Video URL"
                pack
        )

    twoDigits :: Int -> String
    twoDigits n
      | n < 10 = '0' : show n
      | otherwise = show n
