
module Main where

import Control.Applicative
import Control.MILP
import Control.MILP.Types


main :: IO ()
main = putStrLn . show =<< minimize program


program :: LP ()
program = do
  x1 <- general
  x2 <- general
  objective $ x1 + x2
  4 * x1 + x2 >=^ 5
  -- x1 <=^ 5
  x1 <=^ 4 <|> x2 <=^ 6
