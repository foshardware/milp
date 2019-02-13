{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


newline :: Builder
newline = "\n"

programBuilder :: Int -> Int -> Program -> Builder
programBuilder bins gens (Program a s bs)
   = objectiveBuilder a
  <> "SUBJECT TO" <> newline
  <> subjectToBuilder 1 s
  <> "BOUNDS" <> newline
  <> mconcat (fmap boundBuilder bs)
  <> "INTEGERS" <> newline
  <> mconcat (fmap generalBuilder [1 .. gens])
  <> "BINARIES" <> newline
  <> mconcat (fmap binaryBuilder [1 .. bins])
  <> "END" <> newline

generalBuilder :: Int -> Builder
generalBuilder n = " x" <> decimal n <> newline

binaryBuilder :: Int -> Builder
binaryBuilder n = " y" <> decimal n <> " z" <> decimal n <> newline

objectiveBuilder :: Objective -> Builder
objectiveBuilder (Objective e) = " objective: " <> expBuilder e <> newline

subjectToBuilder :: Int -> SubjectTo -> Builder
subjectToBuilder c (Cont a b) = subjectToBuilder c a <> subjectToBuilder c b
subjectToBuilder c (Equal a b)
  = " s" <> ": " <> expBuilder a <> " = " <> expBuilder b <> newline
subjectToBuilder c (LessEq a b)
  = " s" <> ": " <> expBuilder a <> " <= " <> expBuilder b <> newline
subjectToBuilder c (GreaterEq a b)
  = " s" <> ": " <> expBuilder a <> " >= " <> expBuilder b <> newline
subjectToBuilder _ _ = mempty

boundBuilder :: Bound -> Builder
boundBuilder (Bound a b x)
  = decimal a <> " <= " <> expBuilder x <> " <= " <> decimal b <> newline

expBuilder :: Exp -> Builder
expBuilder (Sym x) = "x" <> decimal x
expBuilder (Bin y) = "y" <> decimal y
expBuilder (Bin' z) = "z" <> decimal z
expBuilder      M  = decimal constantM
expBuilder (Lit n) = decimal n
expBuilder (Neg (Neg x)) = expBuilder x
expBuilder (Neg x) = "- " <> expBuilder x
expBuilder (Add a b) = expBuilder a <> " + " <> expBuilder b
expBuilder (Sub a b) = expBuilder a <> " - " <> expBuilder b
expBuilder (Mul (Lit n) (Lit k)) = decimal (n * k)
expBuilder (Mul (Lit n) b) = decimal n <> " " <> expBuilder b
expBuilder (Mul b (Lit n)) = decimal n <> " " <> expBuilder b
expBuilder (Mul M b) = expBuilder M <> " " <> expBuilder b
expBuilder e = error $ show e

