{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map as M
import Control.Monad.State
import CHR2.AST.Untyped
import CHR2.Parser
import CHR2.Compile
import CHR2.Target.PostgreSQL
import GHC.Environment

makeSql :: String -> IO ()
makeSql n = do
  e@Env{..} <- execStateT (do {parse fn; initEnv}) emptyEnv{_name = n}
  publishScript e
  where
    fn = n ++ ".chr"

main :: IO ()
main = do
  args <- getFullArgs
  mapM_ makeSql args


