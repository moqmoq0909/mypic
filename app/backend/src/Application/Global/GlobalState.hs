module Application.Global.GlobalState
  ( GlobalState (..),
    readWorker,
    connectWorker,
    disconnectedWorker,
  )
where

import qualified Application.Global.WSWorker as WSW
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Util ((|>))

-- 型シノニム(同義語)を作成
type WSWorkerMap = Map.Map WSW.Id (Maybe WSW.WSWorker)

-- getWSWorker :: WSWorkerMap -> WSW.Id -> Maybe (Maybe WSW.WSWorker)
-- getWSWorker wsworkers wid = wsworkers |>  Map.lookup wid

newtype GlobalState = GlobalState
  { stmWSWorkerMap :: STM.TVar WSWorkerMap
  }

readWorker :: GlobalState -> WSW.Id -> IO (Maybe (Maybe WSW.WSWorker))
readWorker gs wid = STM.atomically $ do
  webSocketWorkers <- gs |> stmWSWorkerMap |> STM.readTVar
  -- getWSWorker webSocketWorkers wid
  return $ webSocketWorkers |> Map.lookup wid

-- STM操作をatomically関数(STMモナド)でラップし、returnでIOモナドに持ち上げる
connectWorker :: GlobalState -> WS.Connection -> IO WSW.WSWorker
connectWorker gs conn = STM.atomically $ do
  webSocketWorkers <- gs |> stmWSWorkerMap |> STM.readTVar

  let maxId = Map.size webSocketWorkers
  let worker = WSW.new (fromIntegral $ maxId + 1) conn

  STM.modifyTVar (gs |> stmWSWorkerMap) (Map.insert (worker |> WSW.id) (Just worker))
  return worker

disconnectedWorker :: GlobalState -> WSW.Id -> IO ()
disconnectedWorker gs wid =
  STM.atomically $ STM.modifyTVar (gs |> stmWSWorkerMap) (Map.update (\_ -> Just Nothing) wid)