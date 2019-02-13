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

import System.IO (hClose, stdout)
import System.IO.Temp
import System.Process

import Text.Parsec (parse)


buildLP :: LP Builder
buildLP = do
  optimize
  s <- lps
  pure $ programBuilder (yTicket s) (xTicket s) (sProgram s)


minimize, maximize :: LP a -> IO (a, Result)
minimize p = checkLP p $ "MINIMIZE" <> newline
maximize p = checkLP p $ "MAXIMIZE" <> newline


checkLP :: LP a -> Builder -> IO (a, Result)
checkLP p prefix = do

  let contents = toLazyText $ newline <> prefix <> runLP (p *> buildLP)

  out <- liftIO $ do
    withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
      withSystemTempFile "coin-or-out" $ \ o out -> do
        hClose out
        hPutStr in_ contents
        hPutStr stdout contents
        hClose in_
        _ <- readProcess "cbc" [i, "solve", "solu", o] mempty
        readFile o

  let a = runLP p

  case parse result "result" out of
    Left  e -> error $ unpack out
    Right r -> pure (a, r)

