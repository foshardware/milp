{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Types where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Data.Functor.Identity

import Data.Map hiding (empty)

import Prelude hiding (fail, lookup)


bigM :: Integer
bigM = 1000


data Program = Program
  Objective
  [SubjectTo]
  [Bound]

instance Semigroup Program where
  Program o st bs <> Program p tt cs = Program (o <> p) (st <> tt) (bs <> cs)

instance Monoid Program where
  mempty = Program mempty mempty mempty
  mappend = (<>)


newtype Objective = Objective Exp

instance Semigroup Objective where
  Objective (Lit 0) <> a = a
  a <> Objective (Lit 0) = a
  Objective a <> Objective b = Objective (a + b)


instance Monoid Objective where
  mempty = Objective 0
  mappend = (<>)


data SubjectTo
  = Equal Exp Exp
  | LessEq Exp Exp
  | GreaterEq Exp Exp


data Bound = Bound Integer Integer Exp


data Exp
  = Sym Int
  | Bin Int
  | M
  | Lit Integer
  | Neg Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Sub Exp Exp
  deriving (Eq, Ord, Show)


instance Num Exp where

  (+) = Add
  (-) = Sub

  (*) = Mul

  fromInteger = literal

  abs (Neg (Neg e)) = abs e
  abs (Neg e) = e
  abs (Lit n) = literal (abs n)
  abs e = e

  signum (Neg (Neg e)) = signum e
  signum (Neg _) = literal (-1)
  signum (Lit n) = literal (signum n)
  signum e = e



infix 4 <==, >==, ===

(===), (<==), (>==) :: Exp -> Exp -> LP ()
a === b = subjectTo $ Equal a b
a <== b = subjectTo $ LessEq a b
a >== b = subjectTo $ GreaterEq a b
  

data LPS = LPS
  { xTicket  :: Int
  , yTicket  :: Int
  , disjunct :: Int
  , sProgram :: Program
  , sResult  :: Result
  }


type LP = LPT IO

newtype LPT m a = LP { unLP :: StateT [LPS] m a }

instance Functor m => Functor (LPT m) where
  fmap f (LP m) = LP (fmap f m)

instance Monad m => Applicative (LPT m) where
  pure = LP . pure
  LP f <*> LP g = LP (f <*> g)

instance Monad m => Monad (LPT m) where
  return = pure
  m >>= k = LP (unLP m >>= unLP . k)

instance Monad m => MonadFail (LPT m) where
  fail = error

instance Monad m => Alternative (LPT m) where
  empty = LP (undefined <$ put mempty)
  f <|> _ = f

instance Monad m => MonadPlus (LPT m) where
  mzero = empty
  mplus = (<|>)


type Result = Map Exp (Integer, Integer)


lp :: Exp -> LP (Maybe (Integer, Integer))
lp e = lookup e . mconcat . fmap sResult <$> lps

lps :: LP [LPS]
lps = LP get

up :: LPS -> LP ()
up s = LP $ do
  _ : ss <- get
  put $ s : ss


runLP :: LP a -> IO a
runLP m = evalStateT (unLP m) [LPS 1 1 0 mempty mempty]


literal :: Integer -> Exp
literal = Lit


free :: LP Exp
free = do
  s : _ <- lps
  up s { xTicket = 1 + xTicket s }
  pure $ Sym $ xTicket s


prog :: Program -> LP ()
prog q = do
  s : _ <- lps
  up s { sProgram = sProgram s <> q }


objective :: Exp -> LP ()
objective e = prog $ Program (Objective e) mempty mempty


subjectTo :: SubjectTo -> LP ()
subjectTo s = prog $ Program mempty [s] mempty

bound :: Integer -> Integer -> Exp -> LP ()
bound a b x = prog $ Program mempty mempty [Bound a b x]


