{
{-# OPTIONS_GHC -w #-}
module Language.LP.Lexer
  ( lexer
  ) where

import Data.Char (ord)
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Language.LP.Tokens

}

$any     = [.\n\r]
@newline = [\n\r] | \r\n

$space = [\ \t]

@comment = \;[^\r\n]*



-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]

$sign = [\+\-]

@decimalNumber = $sign?$decimalDigit+
@floatingPoint = $decimalDigit+[.]$decimalDigit+


-- Strings

@string_unquoted = (\\. | [^\"\\])*
@string = \" (\\. | [^\"\\])* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  @newline            ;

  ^@comment           ;
  $white              ;

  \;                  ;
  \:                  { tok Tok_Colon      }

  \+                  { tok Tok_Plus       }
  \-                  { tok Tok_Minus      }

  \=                  { tok Tok_Eq         }
  \<=                 { tok Tok_LtEq       }
  \>=                 { tok Tok_GtEq       }

  @decimalNumber      { tok Tok_Decimal    }
  @floatingPoint      { tok Tok_Floating   }
  inf                 { tok Tok_Inf        }

  MINIMIZE            { tok Tok_Minimize   }
  Minimize            { tok Tok_Minimize   }

  objective           { tok Tok_Objective  }
 
  SUBJECT             { tok Tok_Subject    }
  Subject             { tok Tok_Subject    }
  TO                  { tok Tok_To         }
  To                  { tok Tok_To         }

  INTEGERS            { tok Tok_Generals   }
  Integers            { tok Tok_Generals   }
  GENERALS            { tok Tok_Generals   }
  Generals            { tok Tok_Generals   }

  BINARIES            { tok Tok_Binaries   }
  Binaries            { tok Tok_Binaries   }
  Binary              { tok Tok_Binaries   }

  BOUNDS              { tok Tok_Bounds     }

  END                 { tok Tok_End        }
  End                 { tok Tok_End        }

  @simpleIdentifier   { tok Tok_Variable   }

  .                   { tok Tok_Unknown    }

{

wrap :: (T.Text -> TokenName) -> AlexPosn -> T.Text -> P Token
wrap f (AlexPn _ line col) s = pure $ Token (f s) s (Position "" line col)

tok = wrap . const
bstrTok f = wrap (f . T.encodeUtf8)
textTok = wrap
charTok f = wrap (f . T.head)

lexer :: String -> T.Text -> [Token]
lexer file text = normalize =<< evalP (go (alexStartPos, '\n', text `T.snoc` '\n'))
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> pure []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> (:) <$> act pos (T.take len cs) <*> go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)


-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

type P = Identity 

evalP :: P a -> a
evalP = runIdentity 


-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

}

