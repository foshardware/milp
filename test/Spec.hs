{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.MILP
import Control.MILP.Builder
import Control.MILP.Types

import Data.FileEmbed
import qualified Data.Text.Lazy.IO as Lazy
import Data.Text.Lazy.Builder
import Data.Text.Encoding

import Language.LP.Parser (parseLP)


main :: IO ()
main = do

  let simpleLP = parseLP $ decodeUtf8 $(embedFile "sample/simple.lp")

  Lazy.putStrLn $ toLazyText $ build 0 $ either (error . show) (lpBuilder 0 0) simpleLP

  ((x1, x2), result) <- satisfy program
  putStrLn $ show $ (,) <$> result x1 <*> result x2



program :: LP (Var, Var)
program = do
  x1 <- general
  x2 <- general
  x3 <- general
  objective $ x1 + x2
  4 * x1 + x2 >=^ 5 - x1
  0 <=. x3 .<= 8
  x1 =^ 2 <|> x1 =^ 2 <|> x2 =^ 3
  x163 <- general
  (_, z315) <- binary
  x163 + 6000 + 100001 * z315 >=^ x1
  pure (x1, x2)
