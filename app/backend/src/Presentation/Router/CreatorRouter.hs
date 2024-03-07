{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Presentation.Router.CreatorRouter (CreatorAPI, creatorRouter) where

-- import Infrastructure.Repository.Creator.CreatorGateway (findCreatorList)
-- import Data.Text ( pack )
import qualified Application.UseCase.Twitter.MyLikedTweetsUploadUseCase as MyLikedTweetsUploadUseCase
import Control.Monad.IO.Class (liftIO)
import Domain.Model.Table.Table (TableRes)
import Domain.Model.Table.TableRepository
  ( makeTable,
  )
import qualified Infrastructure.Repository.Creator.CreatorRecord as CrR
import Presentation.APIRes (APIRes (APIRes))
import Presentation.Router.CustomHandler (AppM)
import Servant
  ( Get,
    Handler,
    JSON,
    Post,
    ReqBody,
    Server,
    ServerT,
    type (:<|>) (..),
    type (:>),
  )

type CreatorAPI =
  GetTable
    :<|> PostTweetUpload
    :<|> UsersPost

creatorRouter :: ServerT CreatorAPI AppM
creatorRouter =
  makeTable
    :<|> postTweetUpload
    :<|> userPost

-- まとめてexport --

type GetTable = "table" :> ReqBody '[JSON] Int :> Post '[JSON] TableRes

-- getTable :: Int -> Handler TableRes
-- getTable = makeTable

type PostTweetUpload = "tweet-upload" :> ReqBody '[JSON] String :> Post '[JSON] APIRes

postTweetUpload :: String -> AppM APIRes
postTweetUpload twitterUserName = do
  _ <- MyLikedTweetsUploadUseCase.run twitterUserName
  return $ APIRes 200 "成功しました"

type UsersPost = "user" :> "edit" :> Get '[JSON] [CrR.CreatorRecord]

-- "user/edit" :> ReqBody '[JSON] User :> Post '[JSON] User
userPost :: AppM [CrR.CreatorRecord]
userPost =
  return
    [ CrR.CreatorRecord "100" "KenKen",
      CrR.CreatorRecord "101" "KenKen2"
    ]