{-# LANGUAGE OverloadedStrings #-}

module Control.MILP where

import Control.Monad.State

import Data.Map

import Data.Text.Lazy.IO
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

import Prelude hiding (putStrLn, writeFile, readFile, lookup)

import System.IO (hClose)
import System.IO.Temp
import System.Process


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
  | Mul Int Exp
  | Sub Exp Exp
  deriving (Eq, Ord)


newline :: Builder
newline = "\n"

programBuilder :: Program -> Builder
programBuilder (Program a ss bs)
   = objectiveBuilder a
  <> "Subject To" <> newline
  <> mconcat (fmap subjectToBuilder $ zip [1..] ss)
  <> mconcat (fmap boundBuilder bs)
  <> fromString "End" <> newline

objectiveBuilder :: Objective -> Builder
objectiveBuilder (Minimize exp) = "Minimize" <> newline <> " obj: " <> expBuilder exp <> newline
objectiveBuilder (Maximize exp) = "Maximize" <> newline <> " obj: " <> expBuilder exp <> newline 
objectiveBuilder _ = "Maximize" <> newline <> " obj: 1" <> newline

subjectToBuilder :: (Int, SubjectTo) -> Builder
subjectToBuilder (c, Equal a b)
  = " c" <> fromString (show c) <> ": " <> expBuilder a <> " = " <> expBuilder b <> "\n"
subjectToBuilder (c, LessEq a b)
  = " c" <> fromString (show c) <> ": " <> expBuilder a <> " <= " <> expBuilder b <> "\n"
subjectToBuilder (c, GreaterEq a b)
  = " c" <> fromString (show c) <> ": " <> expBuilder a <> " >= " <> expBuilder b <> "\n"

boundBuilder :: Bound -> Builder
boundBuilder (Bound a b x)
  = mempty

expBuilder :: Exp -> Builder
expBuilder (Sym x) = "x" <> decimal x
expBuilder (Lit n) = decimal n
expBuilder (Neg x) = "- " <> expBuilder x
expBuilder (Add a b) = expBuilder a <> " + " <> expBuilder b
expBuilder (Sub a b) = expBuilder a <> " - " <> expBuilder b
expBuilder (Mul x b) = decimal x <> " " <> expBuilder b


buildLP :: LP Builder
buildLP = do
  (_, p, _) <- get
  pure $ programBuilder p


infixl 6 .+, .-

(.+), (.-) :: Exp -> Exp -> Exp
(.+) = Add
(.-) = Sub


infixl 7 .*

(.*) :: Int -> Exp -> Exp
(.*) = Mul


infix 4 .<=, .>=, .=

(.=), (.<=), (.>=) :: Exp -> Exp -> LP ()
a .=  b = subjectTo $ Equal a b
a .<= b = subjectTo $ LessEq a b
a .>= b = subjectTo $ GreaterEq a b
  

type LP = StateT (Int, Program, Result) IO

type Result = Map Exp (Integer, Integer)


lp :: Exp -> LP (Maybe (Integer, Integer))
lp exp = do
  (_, _, r) <- get
  pure $ lookup exp r


checkLP :: LP Result
checkLP = do
  contents <- toLazyText <$> buildLP
  result <- liftIO $ do
    withSystemTempFile "coin-or-in.lp" $ \ i in_ ->
      withSystemTempFile "coin-or-out" $ \ o out -> do
        hClose out
        hPutStr in_ contents
        hClose in_
        readProcess "cbc" [i, "solve", "solu", o] mempty
        readFile o
  modify $ \ (t, p, _) -> (t, p, mempty)
  pure mempty


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
minimize exp = prog $ Program (Minimize exp) mempty mempty
maximize exp = prog $ Program (Maximize exp) mempty mempty


subjectTo :: SubjectTo -> LP ()
subjectTo s = prog $ Program None [s] mempty

bound :: Int -> Int -> Exp -> LP ()
bound a b x = prog $ Program None mempty [Bound a b x]


someFunc :: IO ()
someFunc = putStrLn "someFunc"
