{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Result where

import Control.MILP.Types

import Control.Monad

import Data.Map (fromList)

import Text.Parsec
import Text.Parsec.Text.Lazy

import Text.ParserCombinators.Parsec.Number


result :: Parser Result
result
   = string "Optimal - objective value" *> spaces
  *> many1 digit
  *> char '.'
  *> many1 digit
  *> spaces *> body

body :: Parser Result
body = fromList <$> many1 entry

entry :: Parser (Exp, (Integer, Integer))
entry = (,)
  <$> (many1 digit *> spaces *> var)
  <*> ((,) <$> (spaces *> integer) <*> (spaces *> integer))
  <*  spaces


integer :: Parser Integer
integer
  = join
  $ either (fail . show) pure
  . parse decimal "number" <$> many1 digit

var :: Parser Exp
var
  = join
  $ either (fail . show) (pure . Sym)
  . parse decimal "var" <$> (char 'x' *> many1 digit)

