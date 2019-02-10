
module Main where

import Control.MILP
import Control.MILP.Types


main :: IO ()
main = putStrLn . show =<< runLP program


program :: LP Result
program = do
  x1 <- free
  x2 <- free
  minimize $ x1 .+ x2
  x1 .+ x2 .>= literal 8
  x1 .<= literal 5
  x2 .<= literal 4
  checkLP
 
