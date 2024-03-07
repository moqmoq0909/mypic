{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Application.Global.GlobalState as GS
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import Env (initEnv)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Presentation.Cors (corsConfig)
import Presentation.Router.BaseRouter (app)
import Servant
  ( Proxy (..),
    serve,
  )

-- api :: Proxy BaseAPI
-- api = Proxy

-- app :: Application
-- app = corsConfig $ serve api baseRouter

main :: IO ()
main = do
  -- 環境変数を読み込むための設定
  initEnv

  putStrLn "http://localhost:8081/"

  webSocketWorkers <- STM.newTVarIO Map.empty
  run 8081 $ corsConfig $ app $ GS.GlobalState webSocketWorkers
