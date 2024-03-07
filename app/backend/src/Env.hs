module Env (initEnv) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (void)

initEnv :: IO ()
initEnv = void $ loadFile defaultConfig