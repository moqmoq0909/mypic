{-# LANGUAGE OverloadedStrings #-}

module Presentation.Cors (corsConfig) where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    simpleHeaders,
  )

corsConfig :: Middleware
corsConfig = cors (const $ Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Just (["http://localhost:5173", "http://127.0.0.1:5173"], True),
      corsMethods = ["GET", "POST"],
      corsRequestHeaders = simpleHeaders,
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }