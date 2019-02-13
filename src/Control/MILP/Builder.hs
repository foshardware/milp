{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


type Build = WriterT Builder (Reader Integer)

build :: Integer -> Build () -> Builder
build n b = execWriterT b `runReader` n


newline :: Build ()
newline = tell "\n"


programBuilder :: Int -> Int -> Program -> Build ()
programBuilder bins gens (Program a s bs) = do
  objectiveBuilder a
  tell "SUBJECT TO" *> newline
  subjectToBuilder s
  tell "BOUNDS" *> newline
  sequence_ $ boundBuilder <$> bs
  tell "INTEGERS" *> newline
  sequence_ $ generalBuilder <$> [1 .. gens]
  tell "BINARIES" *> newline
  sequence_ $ binaryBuilder <$> [1 .. bins]
  tell "END" *> newline


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


subjectToBuilder :: SubjectTo -> Build ()
subjectToBuilder (Equal a b) = do
  tell " s: "
  expBuilder a
  tell " = "
  expBuilder b
  newline
subjectToBuilder (LessEq a b) = do
  tell " s: "
  expBuilder a
  tell " <= "
  expBuilder b
  newline
subjectToBuilder (GreaterEq a b) = do
  tell " s: "
  expBuilder a
  tell " >= "
  expBuilder b
  newline
subjectToBuilder (Cont a b) = subjectToBuilder a *> subjectToBuilder b
subjectToBuilder _ = pure ()


boundBuilder :: Bound -> Build ()
boundBuilder (Bound a b x) = do
  tell $ decimal a
  tell " <= "
  expBuilder x
  tell " <= "
  tell $ decimal b
  newline


expBuilder :: Exp -> Build ()
expBuilder (Sym x) = tell $ "x" <> decimal x
expBuilder (Bin y) = tell $ "y" <> decimal y
expBuilder (Bin' z) = tell $ "z" <> decimal z
expBuilder      M  = tell $ decimal constantM
expBuilder (Lit n) = tell $ decimal n
expBuilder (Neg (Neg x)) = expBuilder x
expBuilder (Neg x) = tell "- " *> expBuilder x
expBuilder (Add a b) = expBuilder a *> tell " + " *> expBuilder b
expBuilder (Sub a b) = expBuilder a *> tell " - " *> expBuilder b
expBuilder (Mul (Lit n) (Lit k)) = tell $ decimal (n * k)
expBuilder (Mul (Lit n) b) = tell (decimal n) *> tell " " *> expBuilder b
expBuilder (Mul b (Lit n)) = tell (decimal n) *> tell " " *> expBuilder b
expBuilder (Mul M b) = expBuilder M *> tell " " *> expBuilder b
expBuilder e = fail $ show e
