{-# LANGUAGE OverloadedStrings #-}

module Control.MILP.Builder where

import Control.MILP.Types

import Control.Monad.Reader
import Control.Monad.Writer

import Data.HashSet

import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int


type Set = HashSet

type Build = WriterT Builder (Reader Integer)

build :: Integer -> Build () -> Builder
build n b = execWriterT b `runReader` n


newline :: Build ()
newline = tell "\n"


programBuilder :: Int -> Int -> Program -> Build ()
programBuilder bins gens (Program a s bs) = do
  objectiveBuilder a
  tell "SUBJECT TO" *> newline
  m <- decimal <$> lift ask
  tell " s: M = " *> tell m *> newline
  mapM_ literalBuilder $ allLiterals s
  subjectToBuilder s
  tell "BOUNDS" *> newline
  mapM_ boundBuilder bs
  tell "INTEGERS" *> newline
  mapM_ generalBuilder [1 .. gens]
  tell "BINARIES" *> newline
  mapM_ binaryBuilder [1 .. bins]
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


literalBuilder :: Integer -> Build ()
literalBuilder n = do
  tell " s: "
  tell "l"
  tell $ decimal n
  tell " = "
  tell $ decimal n
  newline

allLiterals :: SubjectTo -> Set Integer
allLiterals s = fromList $ collectLiterals =<< collectExpressions s

collectExpressions :: SubjectTo -> [Exp]
collectExpressions (Cont a b) = collectExpressions a ++ collectExpressions b
collectExpressions (LtEq a b) = [a, b]
collectExpressions (GtEq a b) = [a, b]
collectExpressions   (Eq a b) = [a, b]
collectExpressions _ = []

collectLiterals :: Exp -> [Integer]
collectLiterals (Add a b) = collectLiterals a ++ collectLiterals b
collectLiterals (Sub a b) = collectLiterals a ++ collectLiterals b
collectLiterals (Mul a b) = collectLiterals a ++ collectLiterals b
collectLiterals (Lit n) = [n]
collectLiterals _ = []


subjectToBuilder :: SubjectTo -> Build ()
subjectToBuilder (Eq a b) = do
  tell " s: "
  expBuilder a
  tell " = "
  expBuilder b
  newline
subjectToBuilder (LtEq a b) = do
  tell " s: "
  expBuilder a
  tell " <= "
  expBuilder b
  newline
subjectToBuilder (GtEq a b) = do
  tell " s: "
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

expBuilder M = tell "M"

expBuilder (Sym x) = tell $ "x" <> decimal x

expBuilder (Bin  y) = tell $ "y" <> decimal y
expBuilder (Bin' z) = tell $ "z" <> decimal z

expBuilder (Lit n) = tell $ "l" <> decimal n

expBuilder (Neg (Neg x)) = expBuilder x
expBuilder (Neg x) = tell "- " *> expBuilder x

expBuilder (Add a b) = expBuilder a *> tell " + " *> expBuilder b
expBuilder (Sub a b) = expBuilder a *> tell " - " *> expBuilder b

expBuilder (Mul a b) = expBuilder a *> tell " " *> expBuilder b

