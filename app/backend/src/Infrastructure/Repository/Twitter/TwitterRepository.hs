{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Twitter.TwitterRepository (findUserLikeTweets) where

import Domain.Model.Book.BookEntity
  ( BookEntity,
  )
import Infrastructure.ConnectionTwitterAPI (connectTwitterAPI)
import Infrastructure.Repository.Twitter.TwitterLikeGateway (getLikedTweets)
import qualified Infrastructure.Repository.Twitter.TwitterLikeRecord as TLR
import Infrastructure.Repository.Twitter.TwitterUserGateway (getUserByUserName)
import Infrastructure.Repository.Twitter.TwitterUserRecord (userId)

findUserLikeTweets :: String -> IO [BookEntity]
findUserLikeTweets userName = do
  -- -- TwitterAPIで必要なトークンの取得
  bearerToken <- connectTwitterAPI
  case bearerToken of
    Left err -> do
      putStrLn $ "TwitterAPIのトークン取得に失敗しました : " ++ err
      return []
    Right token -> do
      -- ユーザー情報を取得
      -- ※鍵アカだと情報を取得できない
      user <- getUserByUserName token userName
      case user of
        Left err -> do
          putStrLn $ "ユーザーが存在しません" ++ err
          return []
        Right u -> do
          -- 特定のユーザーがいいねしたツイートを取得
          likedTweets <- getLikedTweets token $ userId u
          case likedTweets :: Either String TLR.TwitterResponse of
            Left err -> do
              putStrLn $ "JSONのパースに失敗しました : " ++ err
              return []
            Right twitterResponse -> return $ TLR.toBookEntities twitterResponse