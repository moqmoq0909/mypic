module Application.UseCase.Twitter.MyLikedTweetsUploadUseCase (run) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (lefts, rights)
import qualified Domain.DomainService.SaveImagesDomainService as SaveImagesDomainService
import qualified Infrastructure.Repository.Book.BookGateway as BookGateway
import qualified Infrastructure.Repository.MinIO.S3Repository as S3Repository
import qualified Infrastructure.Repository.Twitter.TwitterRepository as TwitterRepository
import Presentation.Router.CustomHandler (AppM)
import Util ((|>))

run :: String -> AppM ()
run twitterUserName = do
  books <- TwitterRepository.findUserLikeTweets twitterUserName |> liftIO

  uploadedBooks <-
    books
      |> SaveImagesDomainService.generateFilePath
      |> S3Repository.multiUpload

  -- 全ての画像アップロードに成功したBookEntityのみを配列にまとめる
  let successUploads = rights uploadedBooks

  -- 成功したBookEntityの情報をDBに保存
  _ <- BookGateway.insertBooks successUploads |> liftIO
  return ()

-- TODO MinIOへのアップロードに成功した画像のMinIOのファイルパスとtwitterIdを取得し、
-- 1 MySQLのBookテーブルに挿入する処理
-- 2 Twitterの「いいね」を外す処理