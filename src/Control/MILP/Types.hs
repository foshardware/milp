{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Types where

import Control.Monad.State

import Data.Map

import Prelude hiding (lookup)


data Program = Program
  Objective
  [SubjectTo]
  [Bound]

instance Semigroup Program where
  Program o st bs <> Program p tt cs = Program (o <> p) (st <> tt) (bs <> cs)

instance Monoid Program where
  mempty = Program mempty mempty mempty
  mappend = (<>)


data Objective
  = Minimize Exp
  | Maximize Exp
  | None

instance Semigroup Objective where
  None <> o = o
  o <> None = o
  Minimize e <> Minimize f = Minimize (Add e f)
  Maximize e <> Maximize f = Maximize (Add e f)
  _ <> _ = error "maximize or minimize?"


instance Monoid Objective where
  mempty = None
  mappend = (<>)


data SubjectTo
  = Equal Exp Exp
  | LessEq Exp Exp
  | GreaterEq Exp Exp


data Bound = Bound Int Int Exp


data Exp
  = Sym Int
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



infix 4 <=@, >=@, ==@

(==@), (<=@), (>=@) :: Exp -> Exp -> LP ()
a ==@ b = subjectTo $ Equal a b
a <=@ b = subjectTo $ LessEq a b
a >=@ b = subjectTo $ GreaterEq a b
  

type LP = StateT (Int, Program, Result) IO

type Result = Map Exp (Integer, Integer)


lp :: Exp -> LP (Maybe (Integer, Integer))
lp e = do
  (_, _, r) <- get
  pure $ lookup e r


runLP :: LP a -> IO a
runLP m = evalStateT m (1, mempty, mempty)


literal :: Integer -> Exp
literal = Lit


free :: LP Exp
free = do
  (ticket, p, r) <- get
  put (succ ticket, p, r)
  pure $ Sym ticket


prog :: Program -> LP ()
prog q = do
  (t, p, r) <- get
  put (t, p <> q, r)


minimize, maximize :: Exp -> LP ()
minimize e = prog $ Program (Minimize e) mempty mempty
maximize e = prog $ Program (Maximize e) mempty mempty


subjectTo :: SubjectTo -> LP ()
subjectTo s = prog $ Program None [s] mempty

bound :: Int -> Int -> Exp -> LP ()
bound a b x = prog $ Program None mempty [Bound a b x]


