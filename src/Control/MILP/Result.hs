{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Result where

import Control.MILP.Types

import Control.Monad

import Data.HashMap.Lazy
import Prelude hiding (lookup)

import Text.Parsec
import Text.Parsec.Text.Lazy

import Text.ParserCombinators.Parsec.Number




smtResult :: Parser Result
smtResult = do
  string "sat" *> spaces
  flip lookup . fromList <$> many1 smtPoint

smtPoint :: Parser (Var, Integer)
smtPoint = string "(=" >> (,)
  <$> (spaces *> point <* spaces)
  <*> (negInteger <|> integer)
  <*  char ')'
  <*  spaces

negInteger :: Parser Integer
negInteger
  =  char '('
  *> char '-'
  *> spaces
  *> fmap negate integer
  <* char ')'
  <* spaces


lpResult :: Parser Result
lpResult = do
  string "name,solution" *> spaces
  body

body :: Parser Result
body = flip lookup . fromList <$> many1 entry

entry :: Parser (Var, Integer)
entry = (,) <$> point <*> (char ',' *> integer) <* spaces

point :: Parser Var
point = bin <|> bin' <|> var <|> lit <|> M <$ char 'M'


integer :: Parser Integer
integer = variable id "integer" $ pure ()


lit :: Parser Exp
lit = variable Lit "lit" $ char 'l'

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
