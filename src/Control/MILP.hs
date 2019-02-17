{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where

import Control.MILP.Builder
import Control.MILP.Result
import Control.MILP.Types


import Control.Monad.Morph
import Control.Monad.Writer

import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Builder

import Prelude hiding (readFile)

import System.IO (hClose, stderr)
import System.IO.Temp
import System.Process

import Text.Parsec (parse)


minimize, maximize :: LP a -> IO (a, Result)
minimize p = checkLP p $ tell "MINIMIZE"
maximize p = checkLP p $ tell "MAXIMIZE"

checkLP :: LP a -> Build () -> IO (a, Result)
checkLP p desc = evalLP start $ (,)
  <$> hoist generalize p
  <*> checkLPT desc


minimizeIO, maximizeIO :: MonadIO m => LPT m Result
minimizeIO = checkLPT $ tell "MINIMIZE"
maximizeIO = checkLPT $ tell "MAXIMIZE"

checkLPT :: MonadIO m => Build () -> LPT m Result
checkLPT description = do

  optimize

  s <- lps
  m <- findM

  out <- liftIO $ pipe $ toLazyText $ build m $ do
    description *> newline
    programBuilder (yTicket s) (xTicket s) (sProgram s)

  case parse result "result" out of
    Left  e -> fail $ show e
    Right r -> pure r


pipe :: Text -> IO Text
pipe contents = do
  withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
    withSystemTempFile "coin-or-out" $ \ o out -> do
      hClose out
      hPutStr in_ contents
      hPutStr stderr contents
      hClose in_
      _ <- readProcess "cbc" [i, "solve", "solu", o] mempty
      result <- readFile o
      hPutStr stderr result
      pure result

