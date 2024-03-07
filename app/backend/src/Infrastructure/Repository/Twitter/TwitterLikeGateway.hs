{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Twitter.TwitterLikeGateway (getLikedTweets) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Infrastructure.Repository.Twitter.TwitterLikeRecord (TwitterResponse)
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    setRequestHeader,
  )

-- 特定のユーザーの「いいね」したツイートを取得する
getLikedTweets :: BC.ByteString -> String -> IO (Either String TwitterResponse)
getLikedTweets bearerToken uId = do
  initRequest <- parseRequest $ "GET https://api.twitter.com/2/users/" ++ uId ++ "/liked_tweets?max_results=5&tweet.fields=attachments&expansions=attachments.media_keys&media.fields=url,variants,alt_text,preview_image_url,duration_ms,public_metrics"
  -- ++ "&pagination_token=7140dibdnow9c7btw483gx7j0p9wdms8brzdmf4z6wbw2"
  let request = setRequestHeader "Authorization" ["Bearer " `BC.append` bearerToken] initRequest
  response <- httpLBS request
  let json = getResponseBody response

  putStrLn ""
  putStrLn "----いいねしたツイート情報----"
  BLC.putStrLn json

  return $ eitherDecode json