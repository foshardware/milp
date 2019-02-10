
module Main where

import Control.MILP

import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO

import Prelude hiding (putStrLn)


main :: IO ()
main = putStrLn . toLazyText =<< runLP program


program :: LP Builder
program = do
  x4 <- free
  x5 <- free
  minimize x4
  x4 .+ x5 .>= literal 2
  checkLP
  buildLP
 
