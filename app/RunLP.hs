{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Writer

import Data.Text.IO as Strict
import Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder

import System.IO (stderr)

import Control.MILP
import Control.MILP.Builder

import Language.LP.Parser (parseLP)


main :: IO ()
main = do
  lpProgram <- parseLP <$> Strict.getContents
  result <- pipeCbc $ toLazyText $ build 0 $ do
    tell "minimize"
    either (error . show) (lpBuilder 0 0) lpProgram
  Lazy.hPutStrLn stderr result

