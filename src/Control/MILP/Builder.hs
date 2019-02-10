{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


newline :: Builder
newline = "\n"

programBuilder :: Program -> Builder
programBuilder (Program a ss bs)
   = objectiveBuilder a
  <> "Subject To" <> newline
  <> mconcat (fmap subjectToBuilder $ zip [1..] ss)
  <> mconcat (fmap boundBuilder bs)
  <> "End" <> newline

objectiveBuilder :: Objective -> Builder
objectiveBuilder (Minimize e) = "Minimize" <> newline <> " obj: " <> expBuilder e <> newline
objectiveBuilder (Maximize e) = "Maximize" <> newline <> " obj: " <> expBuilder e <> newline 
objectiveBuilder _ = "Maximize" <> newline <> " obj: 1" <> newline

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
expBuilder (Neg x) = "- " <> expBuilder x
expBuilder (Add a b) = expBuilder a <> " + " <> expBuilder b
expBuilder (Sub a b) = expBuilder a <> " - " <> expBuilder b
expBuilder (Mul x b) = decimal x <> " " <> expBuilder b

