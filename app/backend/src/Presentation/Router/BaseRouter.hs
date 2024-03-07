{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Presentation.Router.BaseRouter (app) where

import qualified Application.Global.GlobalState as GS
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.Wai (Application)
import Presentation.Router.CreatorRouter (CreatorAPI, creatorRouter)
import Presentation.Router.CustomHandler (AppM)
import Presentation.Router.WebSocketRouter (WebSocketAPI, webSocketRouter)
import Servant
  ( Handler,
    Proxy (..),
    ServerT,
    hoistServer,
    serve,
    type (:<|>) (..),
  )

type BaseAPI =
  CreatorAPI
    :<|> WebSocketAPI

baseRouter :: ServerT BaseAPI AppM
baseRouter =
  creatorRouter
    :<|> webSocketRouter

-- グローバルな値を各ルーティング先の処理で呼び出せるように、カスタムモナドをHandlerモナドに変換する処理を挿入
api :: Proxy BaseAPI
api = Proxy

nt :: GS.GlobalState -> AppM a -> Handler a
nt s x = runReaderT x s

app :: GS.GlobalState -> Application
app gs = serve api $ hoistServer api (nt gs) baseRouter