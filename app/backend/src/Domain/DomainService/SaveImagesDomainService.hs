{-# LANGUAGE OverloadedStrings #-}

module Domain.DomainService.SaveImagesDomainService (generateFilePath) where

import Control.Monad (forM)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import qualified Data.Text as T
import Domain.Model.Book.BookEntity
  ( BookEntity (..),
  )
import Domain.Model.Image.ImageEntity
  ( ImageEntity (..),
    extractFileName,
    -- setImageUrl,
  )
import Util ((|>))

generateFilePath :: [BookEntity] -> [BookEntity]
generateFilePath bs = runST $ do
  forM bs $ \b -> do
    imgs <- forM (b |> images) $ \img -> do
      let url = img |> originalImgUrl
      let path = (b |> bookKey) <> "/" <> (img |> imageKey) <> "/" <> extractFileName url
      imgRef <- newSTRef img -- STRef を新規作成
      modifySTRef imgRef (\imgEntity -> imgEntity {filePath = Just $ T.pack path}) -- URL を更新
      readSTRef imgRef -- STRef を返す
    bookRef <- newSTRef b
    modifySTRef bookRef (\bookEntity -> bookEntity {images = imgs})
    readSTRef bookRef
