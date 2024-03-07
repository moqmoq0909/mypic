{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Presentation.Router.WebSocketRouter (WebSocketAPI, webSocketRouter) where

-- type (:<|>) (..),

import qualified Application.Global.GlobalState as GS
import qualified Application.Global.WSWorker as WSW
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Network.HTTP.Types.Status (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsApp)
import qualified Network.WebSockets as WS
import Presentation.Router.CustomHandler (AppM)
import Servant
  ( Raw,
    Server,
    ServerT,
    Tagged (..),
    type (:>),
  )
import Servant.API.WebSocket (WebSocket)
import Util ((|>))

-- type WebSocketAPI = WebSocket

-- webSocketRouter :: Server WebSocketAPI
-- webSocketRouter = Tagged websocketHandler

-- type WebSocket = "ws" :> Raw

-- websocketHandler :: Application
-- websocketHandler req respond = case websocketsApp WS.defaultConnectionOptions wsApp req of
--   Just res -> res
--   Nothing -> respond $ responseLBS status400 [] "Not a WebSocket request"

-- wsApp :: WS.ServerApp
-- wsApp pending = do
--   conn <- WS.acceptRequest pending
--   WS.withPingThread conn 30 (return ()) $ do
--     -- ここでWebSocket通信を処理
--     msg <- WS.receiveData conn
--     WS.sendTextData conn (msg :: Text)

type WebSocketAPI = "stream" :> WebSocket

webSocketRouter :: ServerT WebSocketAPI AppM
webSocketRouter = streamData
  where
    streamData :: WS.Connection -> AppM ()
    streamData conn = do
      gs <- ask
      -- IOモナドをReaderTモナド(AppM)に持ち上げる
      worker <- liftIO $ GS.connectWorker gs conn
      -- ※IO()を返すliftIO自体を返さないとApp()を返す処理にならないので注意
      -- ピンポンメッセージのやり取りが30秒無いとタイムアウトするので、10秒間隔でピンポンする
      liftIO $ WS.withPingThread conn 10 (return ()) $ do
        -- スレッドの実行を1秒間待機する処理を無限ループし、websocket接続が切れたタイミングでGlobalStateの接続情報を削除する
        forever $ threadDelay 1000000 `finally` GS.disconnectedWorker gs (worker |> WSW.id)

-- game :: MonadIO m => WS.PendingConnection -> m ()
-- game pending = do
--   conn <- liftIO $ acceptRequest pending
--   liftIO $ withPingThread conn 30 (return ()) $ do
--     forever $ do
--       msg <- receiveData conn :: IO Text
--       sendTextData conn msg