{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


newline :: Builder
newline = "\n"

programBuilder :: Program -> Builder
programBuilder (Program a ss bs)
   = objectiveBuilder a
  <> "Subject To" <> newline
  <> mconcat (fmap subjectToBuilder $ zip [1..] ss)
  <> "Bounds" <> newline
  <> mconcat (fmap boundBuilder bs)
  <> "End" <> newline

objectiveBuilder :: Objective -> Builder
objectiveBuilder (Objective e) = " obj: " <> expBuilder e <> newline

subjectToBuilder :: (Int, SubjectTo) -> Builder
subjectToBuilder (c, Equal a b)
  = " c" <> decimal c <> ": " <> expBuilder a <> " = " <> expBuilder b <> newline
subjectToBuilder (c, LessEq a b)
  = " c" <> decimal c <> ": " <> expBuilder a <> " <= " <> expBuilder b <> newline
subjectToBuilder (c, GreaterEq a b)
  = " c" <> decimal c <> ": " <> expBuilder a <> " >= " <> expBuilder b <> newline

boundBuilder :: Bound -> Builder
boundBuilder (Bound a b x)
  = decimal a <> " <= " <> expBuilder x <> " <= " <> decimal b

expBuilder :: Exp -> Builder
expBuilder (Sym x) = "x" <> decimal x
expBuilder (Lit n) = decimal n
expBuilder (Neg (Neg x)) = expBuilder x
expBuilder (Neg x) = "- " <> expBuilder x
expBuilder (Add a b) = expBuilder a <> " + " <> expBuilder b
expBuilder (Sub a b) = expBuilder a <> " - " <> expBuilder b
expBuilder (Mul (Lit n) (Lit k)) = decimal (n * k)
expBuilder (Mul (Lit n) b) = decimal n <> " " <> expBuilder b
expBuilder (Mul b (Lit n)) = decimal n <> " " <> expBuilder b
expBuilder (Mul a b) = error $ unpack $ toLazyText $ expBuilder a <> " * " <> expBuilder b

