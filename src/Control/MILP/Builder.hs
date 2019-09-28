{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)

import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


type Build = WriterT Builder (Reader Integer)

build :: Integer -> Build () -> Builder
build n b = execWriterT b `runReader` n

build_ :: Build () -> Builder
build_ = build 0

newline :: Build ()
newline = tell "\n"


lpBuilder :: Int -> Int -> Program -> Build ()
lpBuilder binT genT (Program a s bs gens bins) = do
  objectiveBuilder a
  tell "SUBJECT TO" *> newline
  evalStateT (subjectToBuilder s) 0
  tell "BOUNDS" *> newline
  mapM_ boundBuilder bs
  tell "INTEGERS" *> newline
  mapM_ variableBuilder gens
  mapM_ generalBuilder [1 .. genT]
  tell "BINARIES" *> newline
  mapM_ variableBuilder bins
  mapM_ binaryBuilder [1 .. binT]
  tell "END" *> newline


variableBuilder :: Var -> Build ()
variableBuilder (Named x) = do
  tell " "
  tell $ fromText x
  newline
variableBuilder _ = pure ()


generalBuilder :: Int -> Build ()
generalBuilder n = do
  tell " x" 
  tell $ decimal n
  newline


binaryBuilder :: Int -> Build ()
binaryBuilder n = do
  tell " y"
  tell $ decimal n
  tell " z"
  tell $ decimal n
  newline


objectiveBuilder :: Objective -> Build ()
objectiveBuilder (Objective e) = do
  tell " objective: "
  expBuilder e
  newline


subjectToBuilder :: SubjectTo -> StateT Int Build ()
subjectToBuilder (Eq a b) = do
  c <- modify succ *> get
  lift $ do
    tell $ " c" <> decimal c <> ": "
    expBuilder a
    tell " = "
    expBuilder b
    newline
subjectToBuilder (LtEq a b) = do
  c <- modify succ *> get
  lift $ do
    tell $ " c" <> decimal c <> ": "
    expBuilder a
    tell " <= "
    expBuilder b
    newline
subjectToBuilder (GtEq a b) = do
  c <- modify succ *> get
  lift $ do
    tell $ " c" <> decimal c <> ": "
    expBuilder a
    tell " >= "
    expBuilder b
    newline
subjectToBuilder (Cont a b) = subjectToBuilder a *> subjectToBuilder b
subjectToBuilder _ = pure ()


boundBuilder :: Bound -> Build ()
boundBuilder (Bound a b x) = do
  tell " "
  tell $ decimal a
  tell " <= "
  expBuilder x
  tell " <= "
  tell $ decimal b
  newline


expBuilder :: Exp -> Build ()

expBuilder M = tell . decimal =<< lift ask

expBuilder (Sym x) = tell $ "x" <> decimal x
expBuilder (Named x) = tell $ fromText x

expBuilder (Bin  y) = tell $ "y" <> decimal y
expBuilder (Bin' z) = tell $ "z" <> decimal z

expBuilder (Lit n) = tell $ decimal n

expBuilder (Neg (Neg x)) = expBuilder x
expBuilder (Neg x) = tell "- " *> expBuilder x

expBuilder (Add a b) = expBuilder a *> tell " + " *> expBuilder b
expBuilder (Sub a b) = expBuilder a *> tell " - " *> expBuilder b

expBuilder (Mul a b) = expBuilder a *> tell " " *> expBuilder b




smtBuilder :: Int -> Int -> Program -> Build ()
smtBuilder bins gens (Program _ s bs _ _) = do
  tell "(set-option :produce-models true)" *> newline
  tell "(set-logic QF_LIA)" *> newline
  newline
  mapM_ constBuilder [1 .. gens]
  newline
  mapM_ constBinBuilder [1 .. bins]
  newline
  mapM_ constBoundBuilder [1 .. gens]
  newline
  mapM_ constBinBoundBuilder [1 .. bins]
  newline
  mapM_ varBoundBuilder bs
  newline
  assertionBuilder s
  newline
  tell "(check-sat)" *> newline
  newline
  tell "(get-model)" *> newline
  newline
  tell "(exit)" *> newline


assertionBuilder :: SubjectTo -> Build ()

assertionBuilder a @ (Alt _ _) = do
  tell "(assert "
  assertionExpression a
  tell ")"
  newline
assertionBuilder (Cont a b) = do
  assertionBuilder a
  assertionBuilder b

assertionBuilder (Eq a b) = do
  tell "(assert (= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell "))"
  newline
assertionBuilder (LtEq a b) = do
  tell "(assert (<= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell "))"
  newline
assertionBuilder (GtEq a b) = do
  tell "(assert (>= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell "))"
  newline
assertionBuilder _ = pure ()


assertionExpression :: SubjectTo -> Build ()

assertionExpression (Cont a b) = do
  tell "(and "
  assertionExpression a
  tell " "
  assertionExpression b
  tell ")"
assertionExpression Unit = tell "true"

assertionExpression (Alt a b) = do
  tell "(or "
  assertionExpression a
  tell " "
  assertionExpression b
  tell ")"
assertionExpression Zero = tell "false"

assertionExpression (Eq a b) = do
  tell "(= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"
assertionExpression (LtEq a b) = do
  tell "(<= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"
assertionExpression (GtEq a b) = do
  tell "(>= "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"


stmtBuilder :: Exp -> Build ()
stmtBuilder (Sym x) = tell $ "x" <> decimal x
stmtBuilder (Lit n) | n < 0 = stmtBuilder (Neg (Lit (abs n)))
stmtBuilder (Lit n) = tell $ decimal n
stmtBuilder (Bin  n) = tell $ "y" <> decimal n
stmtBuilder (Bin' n) = stmtBuilder (Neg (Bin n))
stmtBuilder (Neg (Neg x)) = stmtBuilder x
stmtBuilder (Neg (Bin  x)) = tell "(- 1 " *> stmtBuilder (Bin x) *> tell ")"
stmtBuilder (Neg (Bin' x)) = stmtBuilder (Bin x)
stmtBuilder (Neg (Lit n)) = tell $ "(- " <> decimal n <> ")"
stmtBuilder (Neg x) = tell "(0 - " *> stmtBuilder x *> tell ")"
stmtBuilder (Add a b) = do
  tell "(+ "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"
stmtBuilder (Sub a b) = do
  tell "(- "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"
stmtBuilder (Mul a b) = do
  tell "(* "
  stmtBuilder a
  tell " "
  stmtBuilder b
  tell ")"
stmtBuilder _ = pure ()


varBoundBuilder :: Bound -> Build ()
varBoundBuilder (Bound l u v) = do
  tell "(assert (<= "
  tell $ decimal l
  tell " "
  stmtBuilder v
  tell " "
  tell $ decimal u
  tell "))"
  newline


constBoundBuilder :: Int -> Build ()
constBoundBuilder n = do
  tell $ "(assert (<= 0 x" <> decimal n <> "))"
  newline

constBinBoundBuilder :: Int -> Build ()
constBinBoundBuilder n = do
  tell $ "(assert (<= 0 y" <> decimal n <> " 1))"
  newline


constBuilder :: Int -> Build ()
constBuilder n = do
  tell $ "(declare-fun x" <> decimal n <> " () Int)"
  newline

constBinBuilder :: Int -> Build ()
constBinBuilder n = do
  tell $ "(declare-fun y" <> decimal n <> " () Int)"
  newline
