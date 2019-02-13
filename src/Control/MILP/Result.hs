{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Result where

import Control.MILP.Types

import Control.Monad

import Data.HashMap.Lazy
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
body = flip lookup . fromList <$> many1 entry

entry :: Parser (Exp, Integer)
entry = (,)
  <$> (many1 digit *> spaces *> point <* spaces)
  <*> (integer <* spaces <* integer <* spaces)

point :: Parser Var
point = bin <|> bin' <|> var


integer :: Parser Integer
integer = variable id "integer" $ pure ()

var :: Parser Var
var = variable Sym "var" $ char 'x'

bin :: Parser Var
bin = variable Bin "bin" $ char 'y'

bin' :: Parser Var
bin' = variable Bin' "bin" $ char 'z'

variable :: Integral n => (n -> b) -> String -> Parser a -> Parser b
variable sym desc prec
  = join
  $ either (fail . show) (pure . sym)
  . parse decimal desc <$> (prec *> many1 digit)

{-# SPECIALIZE variable :: (Int -> Var)         -> String -> Parser Char -> Parser Var     #-}
{-# SPECIALIZE variable :: (Integer -> Integer) -> String -> Parser ()   -> Parser Integer #-}
