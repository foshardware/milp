{

module Language.LP.Parser where

import Control.Exception

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Text.Parsec hiding (try, ParseError(..))
import Text.ParserCombinators.Parsec.Number

import Control.MILP.Types
import Language.LP.Tokens
import Language.LP.Lexer

}

%name program
%tokentype { Token }
%error { parseError }
%monad { Either ParseError } { (>>=) } { return }


%token

"minimize"       { Token Tok_Minimize   _ _ }
"maximize"       { Token Tok_Maximize   _ _ }

"objective"      { Token Tok_Objective  _ _ }

"subject"        { Token Tok_Subject    _ _ }
"to"             { Token Tok_To         _ _ }

"variable"       { Token Tok_Variable   _ _ }
"decimal"        { Token Tok_Decimal    _ _ }
"floating"       { Token Tok_Floating   _ _ }
"inf"            { Token Tok_Inf        _ _ }

"generals"       { Token Tok_Generals   _ _ }
"binaries"       { Token Tok_Binaries   _ _ }
"bounds"         { Token Tok_Bounds     _ _ }

"end"            { Token Tok_End        _ _ }

":"              { Token Tok_Colon      _ _ }

"+"              { Token Tok_Plus       _ _ }
"-"              { Token Tok_Minus      _ _ }

"="              { Token Tok_Eq         _ _ }
"<="             { Token Tok_LtEq       _ _ }
">="             { Token Tok_GtEq       _ _ }

%%

Program :: { Program }
: Goal Objective
  "subject" "to" SubjectTo
  Bounds
  Generals
  Binaries
  opt("end")
  { Program $2 $5 $6 $7 $8 }

Goal :: { () }
: "minimize" { () }
| "maximize" { () }

Objective :: { Objective }
: "objective" ":" Exp { Objective $3 }
| Exp { Objective $1 }


SubjectTo :: { SubjectTo }
: Constraint SubjectTo { Cont $1 $2 }
| Constraint { $1 }

Constraint :: { SubjectTo }
: Variable ":" Exp "=" Number { Eq $3 $5 }
| Variable ":" Exp "<=" Number { LtEq $3 $5 }
| Variable ":" Exp ">=" Number { GtEq $3 $5 }


Generals :: { [Var] }
: "generals" GeneralLenient { $2 }

GeneralLenient :: { [Var] }
: Variable GeneralLenient { Named $1 : $2 }
| "+" GeneralLenient { $2 }
| "-" GeneralLenient { $2 }
| Number GeneralLenient { $2 }
| Variable { [ Named $1] }


Binaries :: { [Var] }
: "binaries" many(Variable) { fmap Named $2 }


Bounds :: { [Bound] }
: "bounds" many(Bound) { $2 }
| { [] }

Bound :: { Bound }
: Decimal "<=" Variable "<=" Decimal { Bound $1 $5 $ Named $3 }


Exp :: { Exp }
: Variable { Named $1 }
| Number { $1 }
| Number Exp { Mul $1 $2 }
| Exp "+" Exp { Add $1 $3 }
| Exp "-" Exp { Sub $1 $3 }


Number :: { Exp }
: Decimal { Lit $1 }
| Floating { Float $1 }
| "inf" { Inf }

Decimal :: { Integer }
: "decimal" { base10 $1 }

Floating :: { Double }
: "floating" { float $1 }

Variable :: { Text }
: "variable" { content $1 }


many(p)
: p many(p) { $1 : $2 }
| { [] } 

opt(p)
: p { Just $1 }
|   { Nothing }


{

parseLP = program . lexer []

data ParseError = Exhausted | Unexpected Token

instance Exception ParseError

instance Show ParseError where
  show Exhausted = "no tokens left to parse."
  show (Unexpected (Token t s p)) = unwords
    [ "unexpected token"
    , "'" ++ unpack s ++ "'", "(" ++ show t ++ ")"
    , "at", show p ++ "."
    ]

parseError :: [Token] -> Either ParseError a
parseError (t : _) = Left $ Unexpected t
parseError _ = Left Exhausted


base10 :: Token -> Integer
base10 = either (error . show) id . parse (sign >>= \s -> s <$> decimal) "base10" . unpack . content

float :: Token -> Double
float = either (error . show) id . parse floating "float" . unpack . content

}

