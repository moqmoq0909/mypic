{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application.Global.WSWorker (Id (..), WSWorker (..), new) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Network.WebSockets as WS

newtype Id = Id Int
  deriving (Show, Eq, Ord, Num, FromJSON, ToJSON)

data WSWorker = WSWorker
  { id :: Id,
    conn :: WS.Connection,
    working :: Bool
  }

new :: Id -> WS.Connection -> WSWorker
new _wid _conn = WSWorker _wid _conn False