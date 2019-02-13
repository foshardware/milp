{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Result where

import Control.MILP.Types

import Control.Monad

import Data.Map (fromList, lookup)
import Prelude hiding (lookup)

import Text.Parsec
import Text.Parsec.Text.Lazy

import Text.ParserCombinators.Parsec.Number


result :: Parser Result
result
   = string "Optimal"
  *> string " - objective value" *> spaces
  *> many1 digit
  *> char '.'
  *> many1 digit
  *> spaces *> body

body :: Parser Result
body = do
  r <- fromList <$> many1 entry
  pure $ flip lookup r

entry :: Parser (Exp, Integer)
entry = (,)
  <$> (many1 digit *> spaces *> point <* spaces)
  <*> (integer <* spaces <* integer <* spaces)

point :: Parser Var
point = bin <|> bin' <|> var


integer :: Parser Integer
integer = variable id "integer" (pure ())

var :: Parser Var
var = variable (Sym . fromIntegral) "var" (char 'x')

bin :: Parser Var
bin = variable (Bin . fromIntegral) "bin" (char 'y')

bin' :: Parser Var
bin' = variable (Bin' . fromIntegral) "bin" (char 'z')


variable :: (Integer -> b) -> String -> Parser a -> Parser b
variable sym desc prec
  = join
  $ either (fail . show) (pure . sym)
  . parse decimal desc <$> (prec *> many1 digit)

