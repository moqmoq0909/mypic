{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository.Twitter.TwitterUserRecord (TwitterUserRecord (..)) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, Value)

-- ユーザー情報を表すデータ型
data TwitterUserRecord = TwitterUserRecord
  { userId :: String,
    displayName :: String,
    userName :: String
  }
  deriving (Show)

instance FromJSON TwitterUserRecord where
  parseJSON :: Value -> Parser TwitterUserRecord
  parseJSON = withObject "User" $ \v -> do
    userData <- v .: "data"
    TwitterUserRecord
      <$> userData .: "id"
      <*> userData .: "name"
      <*> userData .: "username"