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

import Text.Parsec (parse)


buildLP :: LP Builder
buildLP = do
  s : _ <- lps
  pure $ programBuilder $ sProgram s


minimize, maximize :: LP () -> IO Result
minimize p = checkLP p $ "Minimize" <> newline
maximize p = checkLP p $ "Maximize" <> newline


checkLP :: LP () -> Builder -> IO Result
checkLP p prefix = do

  contents <- toLazyText . mappend prefix <$> runLP (p *> buildLP)

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
    Right r -> pure r

