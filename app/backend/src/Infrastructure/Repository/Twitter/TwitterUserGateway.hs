{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Twitter.TwitterUserGateway (getUserByUserName) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Infrastructure.Repository.Twitter.TwitterUserRecord (TwitterUserRecord)
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    setRequestHeader,
  )

-- ユーザー名からユーザー情報を取得する
getUserByUserName :: BC.ByteString -> String -> IO (Either String TwitterUserRecord)
getUserByUserName bearerToken userName = do
  initRequest <- parseRequest $ "GET https://api.twitter.com/2/users/by/username/" ++ userName
  let request = setRequestHeader "Authorization" ["Bearer " `BC.append` bearerToken] initRequest
  response <- httpLBS request
  let json = getResponseBody response

  putStrLn ""
  putStrLn "----ユーザー情報----"
  BLC.putStrLn json

  return $ eitherDecode json