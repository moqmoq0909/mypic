{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.ConnectionTwitterAPI (connectTwitterAPI) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Client (RequestBody (RequestBodyBS), defaultRequest, host, method, path, port, requestBody, requestHeaders, secure)
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import System.Environment (getEnv)

connectTwitterAPI :: IO (Either String BC.ByteString)
connectTwitterAPI = do
  clientId <- getEnv "TWITTER_API_KEY"
  clientSecret <- getEnv "TWITTER_API_KEY_SECRET"
  let credentials = encodeCredentials clientId clientSecret
  getBearerToken credentials

-- Client IDとClient SecretをBase64でエンコードする
encodeCredentials :: String -> String -> BC.ByteString
encodeCredentials clientId clientSecret =
  B64.encode $ BC.concat [BC.pack clientId, ":", BC.pack clientSecret]

getBearerToken :: BC.ByteString -> IO (Either String BC.ByteString)
getBearerToken credentials = do
  let request =
        -- "https://api.twitter.com/2/oauth2/token"
        defaultRequest
          { method = "POST",
            host = "api.twitter.com",
            secure = True, -- HTTPS接続を指定
            port = 443, -- HTTPSの標準ポート
            path = "/oauth2/token",
            requestHeaders =
              [ (hAuthorization, "Basic " `BC.append` credentials),
                (hContentType, "application/x-www-form-urlencoded;charset=UTF-8")
              ],
            requestBody = RequestBodyBS "grant_type=client_credentials"
          }
  response <- httpLBS request
  let json = getResponseBody response

  -- putStrLn "--json--"
  -- BL.putStrLn json

  case eitherDecode json :: Either String (Map String String) of
    Left err -> return $ Left err -- デコード失敗の場合
    Right accessTokenMap -> return . Right . BC.pack $ Map.findWithDefault "" "access_token" accessTokenMap