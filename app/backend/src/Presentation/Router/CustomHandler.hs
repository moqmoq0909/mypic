module Presentation.Router.CustomHandler (AppM) where

import qualified Application.Global.GlobalState as GS
import Control.Monad.Trans.Reader (ReaderT)
import Servant (Handler)

type AppM = ReaderT GS.GlobalState Handler