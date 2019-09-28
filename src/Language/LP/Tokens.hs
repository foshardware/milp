
module Language.LP.Tokens where

import Data.Text (Text)
import Text.Printf

content :: Token -> Text
content (Token _ s _) = s
content _ = mempty

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token
  = Tokens [Token]
  | Token TokenName Text Position
  deriving (Show, Eq)

normalize :: Token -> [Token]
normalize (Tokens ts) = normalize =<< ts
normalize t = [t]


data TokenName
  = Tok_End
  | Tok_Minimize
  | Tok_Maximize
  | Tok_Objective
  | Tok_Generals
  | Tok_Binaries
  | Tok_Bounds
  | Tok_Subject
  | Tok_To
  | Tok_Colon
  | Tok_Plus
  | Tok_Minus
  | Tok_Eq
  | Tok_LtEq
  | Tok_GtEq
  | Tok_Decimal
  | Tok_Variable
  | Tok_Unknown
  deriving (Show, Eq)

