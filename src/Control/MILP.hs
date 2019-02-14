{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where


import Control.MILP.Builder
import Control.MILP.Result
import Control.MILP.Types

import Control.Monad.Writer

import Data.Text.Lazy
import Data.Text.Lazy.IO
import Data.Text.Lazy.Builder

import Prelude hiding (readFile)

import System.IO (hClose, stdout)
import System.IO.Temp
import System.Process

import Text.Parsec (parse)


buildLP :: Monad m => LPT m (Build ())
buildLP = do
  optimize
  s <- lps
  pure $ programBuilder (yTicket s) (xTicket s) (sProgram s)


minimize, maximize :: LP a -> IO (a, Result)
minimize p = checkLP p $ tell "MINIMIZE" *> newline
maximize p = checkLP p $ tell "MAXIMIZE" *> newline

checkLP :: LP a -> Build () -> IO (a, Result)
checkLP p prefix = do

  let (a, m, b) = runLP $ (,,) <$> p <*> findM <*> buildLP

  out <- pipe $ toLazyText $ build m $ newline *> prefix *> b

  case parse result "result" out of
    Left  _ -> error $ unpack out
    Right r -> pure (a, r)


minimizeIO, maximizeIO :: MonadIO m => LPT m Result
minimizeIO = checkLPT $ tell "MINIMIZE" *> newline
maximizeIO = checkLPT $ tell "MAXIMIZE" *> newline

checkLPT :: MonadIO m => Build () -> LPT m Result
checkLPT prefix = do

  m <- findM
  b <- buildLP

  out <- liftIO $ pipe $ toLazyText $ build m $ newline *> prefix *> b

  case parse result "result" out of
    Left  _ -> error $ unpack out
    Right r -> pure r


pipe :: Text -> IO Text
pipe contents = do
  withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
    withSystemTempFile "coin-or-out" $ \ o out -> do
      hClose out
      hPutStr in_ contents
      hPutStr stdout contents
      hClose in_
      _ <- readProcess "cbc" [i, "solve", "solu", o] mempty
      readFile o
