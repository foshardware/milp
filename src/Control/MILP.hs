{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where

import Control.MILP.Builder
import Control.MILP.Result
import Control.MILP.Types

import Control.Applicative

import Control.Monad.Morph
import Control.Monad.Writer

import Data.Maybe
import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Builder

import Prelude hiding (readFile)

import System.IO (hClose, stderr)
import System.IO.Temp
import System.Environment
import System.Process

import Text.Parsec (parse)


satisfy :: LP a -> IO (a, Result)
satisfy p = evalLP start $ (,) <$> hoist generalize p <*> satLPT

satLPT :: MonadIO m => LPT m Result
satLPT = do

  s <- lps

  out <- liftIO $ pipeYices $ toLazyText $ build_ $ do
    smtBuilder (yTicket s) (xTicket s) (sProgram s)

  case parse smtResult "smt" out of
    Left  e -> fail $ show e
    Right r -> pure r


minimize, maximize :: LP a -> IO (a, Result)
minimize p = checkLP p $ tell "MINIMIZE"
maximize p = checkLP p $ tell "MAXIMIZE"

checkLP :: LP a -> Build () -> IO (a, Result)
checkLP p desc = evalLP start $ (,)
  <$> hoist generalize p
  <*> objectiveLPT desc


minimizeIO, maximizeIO :: MonadIO m => LPT m Result
minimizeIO = objectiveLPT $ tell "MINIMIZE"
maximizeIO = objectiveLPT $ tell "MAXIMIZE"

objectiveLPT :: MonadIO m => Build () -> LPT m Result
objectiveLPT description = do

  optimize

  s <- lps
  m <- findM

  out <- liftIO $ pipeCbc $ toLazyText $ build m $ do
    description *> newline
    lpBuilder (yTicket s) (xTicket s) (sProgram s)

  case parse lpResult "lp" out of
    Left  e -> fail $ show e
    Right r -> pure r



pipeYices :: Text -> IO Text
pipeYices contents = do
  debug <- isJust <$> lookupEnv "DEBUG"
  (Just in_, Just out, _, _) <- createProcess (proc "yices-smt2" [])
    { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr in_ contents
  when debug $ hPutStr stderr contents
  temp <- hGetContents out
  when debug $ hPutStr stderr temp
  pure temp


pipeCbc :: Text -> IO Text
pipeCbc contents = do
  withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
    withSystemTempFile "coin-or-out" $ \ o out -> do
      debug <- isJust <$> lookupEnv "DEBUG"
      hClose out
      hPutStr in_ contents
      when debug $ hPutStr stderr contents
      hClose in_
      _ <- readProcess "cbc" [i, "printi", "csv", "solve", "solu", o] mempty
      temp <- readFile o
      when debug $ hPutStr stderr temp
      pure temp
