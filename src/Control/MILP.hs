{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where

import Control.MILP.Builder
import Control.MILP.Result
import Control.MILP.Types

import Control.Monad.State

import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Builder

import Prelude hiding (readFile)

import System.IO (hClose)
import System.IO.Temp
import System.Process

import Text.Parsec


buildLP :: LP Builder
buildLP = do
  (_, p, _) <- get
  pure $ programBuilder p


checkLP :: LP Result
checkLP = do

  contents <- toLazyText <$> buildLP

  out <- liftIO $ do
    withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
      withSystemTempFile "coin-or-out" $ \ o out -> do
        hClose out
        hPutStr in_ contents
        hClose in_
        _ <- readProcess "cbc" [i, "solve", "solu", o] mempty
        readFile o

  case parse result "result" out of
    Left  _ -> error $ unpack out
    Right r -> do
      modify $ \ (t, p, _) -> (t, p, r)
      pure r

