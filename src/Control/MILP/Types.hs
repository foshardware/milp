{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Control.MILP.Types where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Data.Functor.Identity

import Data.Map hiding (empty, drop)

import Prelude hiding (fail, lookup)


bigM :: Integer
bigM = 1000


convexHull :: SubjectTo -> (SubjectTo, [Bound])
convexHull = (, mempty)


data Program = Program Objective SubjectTo [Bound]

instance Semigroup Program where
  Program o st bs <> Program p tt cs
    = Program (o <> p) (conjunction $ Con st <> Con tt) (bs <> cs)

instance Monoid Program where
  mempty = Program mempty (disjunction mempty) mempty
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
  | One
  | Cont SubjectTo SubjectTo
  | Zero
  | Alt SubjectTo SubjectTo


newtype Disjunction = Dis { disjunction :: SubjectTo }

instance Semigroup Disjunction where
  Dis Zero <> a = a
  a <> Dis Zero = a
  Dis a <> Dis b = Dis (Alt a b)

instance Monoid Disjunction where
  mempty = Dis Zero
  mappend = (<>)


newtype Conjunction = Con { conjunction :: SubjectTo }

instance Semigroup Conjunction where
  Con One <> a = a
  a <> Con One = a
  Con a <> Con b = Con (Cont a b)

instance Monoid Conjunction where
  mempty = Con One
  mappend = (<>)



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
  


type Result = Map Exp (Integer, Integer)


data LPS = LPS
  { xTicket  :: Int
  , yTicket  :: Int
  , sProgram :: Program
  , sResult  :: Result
  }


type LP = LPT Identity

newtype LPT m a = LP { unLP :: StateT LPS m a }

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
  empty = LP $ error "empty lp" <$ put (LPS 1 1 mempty mempty)

  f <|> g = LP $ do

    s <- get

    (x, t) <- lift $ runLPT s { sProgram = mempty } f
    (y, u) <- lift $ runLPT t { sProgram = mempty } g

    let Program o st bs = sProgram s
        Program _ tt  _ = sProgram t
        Program _ ut  _ = sProgram u

    let q = disjunction $ Dis tt <> Dis ut
        p = conjunction $ Con st <> Con q

    put u { sProgram = Program o p bs }

    case (tt, ut) of
      (Zero, Zero) -> pure $ error "empty lp"
      (Zero, _) -> pure y
      _ -> pure x


instance Monad m => MonadPlus (LPT m) where
  mzero = empty
  mplus = (<|>)



runLP :: LP a -> a
runLP = fst . runIdentity . runLPT (LPS 1 1 mempty mempty)

runLPT :: Monad m => LPS -> LPT m a -> m (a, LPS)
runLPT s m = runStateT (unLP m) s



lp :: Exp -> LP (Maybe (Integer, Integer))
lp e = lookup e . sResult <$> lps

lps :: Monad m => LPT m LPS
lps = LP get

up :: Monad m => LPS -> LPT m ()
up = LP . put

optimize :: Monad m => LPT m ()
optimize = do
  s <- lps
  let Program o st bs = sProgram s
      (subj, cs) = convexHull st
  up s { sProgram = Program o subj (bs ++ cs) }


literal :: Integer -> Exp
literal = Lit

free :: Monad m => LPT m Exp
free = do
  s <- lps
  up s { xTicket = 1 + xTicket s }
  pure $ Sym $ xTicket s


prog :: Monad m => Program -> LPT m ()
prog q = do
  s <- lps
  up s { sProgram = sProgram s <> q }


objective :: Monad m => Exp -> LPT m ()
objective e = prog $ Program (Objective e) (disjunction mempty) mempty


subjectTo :: Monad m => SubjectTo -> LPT m ()
subjectTo s = prog $ Program mempty s mempty

bound :: Monad m => Integer -> Integer -> Exp -> LPT m ()
bound a b x = prog $ Program mempty (disjunction mempty) [Bound a b x]


