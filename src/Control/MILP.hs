{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where

import Control.MILP.Builder
import Control.MILP.Result
import Control.MILP.Types

import Control.Applicative

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
    Right r -> pure
      $ inferBinaries
      $ inferIntegers (sProgram s)
      $ r


inferBinaries :: Result -> Result
inferBinaries f (Bin  k) = f (Bin  k) <|> (1-) <$> f (Bin' k)
inferBinaries f (Bin' k) = f (Bin' k) <|> (1-) <$> f (Bin  k)
inferBinaries f x = f x

inferIntegers :: Program -> Result -> Result
inferIntegers (Program _ ss _) f (Sym x) = f (Sym x) <|> fromEquality ss f (Sym x)
inferIntegers _ f x = f x

fromEquality :: SubjectTo -> Result -> Result
fromEquality (Cont a b) f x = fromEquality a f x <|> fromEquality b f x
fromEquality (Eq (Sub a b) (Lit n)) f x | x == a = (n +) <$> f b
fromEquality (Eq (Sub a b) (Lit n)) f x | x == b = (\a'->a'-n) <$> f a
fromEquality (Eq (Add a b) (Lit n)) f x | x == a = (n -) <$> f b
fromEquality (Eq (Add a b) (Lit n)) f x | x == b = (n -) <$> f a
fromEquality _ _ _ = Nothing


pipe :: Text -> IO Text
pipe contents = do
  withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
    withSystemTempFile "coin-or-out" $ \ o out -> do
      hClose out
      hPutStr in_ contents
      hPutStr stderr contents
      hClose in_
      _ <- readProcess "cbc" [i, "solve", "solu", o] mempty
      temp <- readFile o
      hPutStr stderr temp
      pure temp

