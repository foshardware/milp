{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.MILP.Types where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Data.Functor.Identity
import Data.Foldable

import Data.Hashable

import GHC.Generics

import Prelude hiding (fail, truncate)


convexHull :: Monad m => SubjectTo -> LPT m SubjectTo

convexHull (Cont p q) = Cont <$> convexHull p <*> convexHull q

convexHull p @ (Alt _ _) = bigM p

convexHull p = pure p


bigM :: Monad m => SubjectTo -> LPT m SubjectTo

bigM (Cont p q) = Cont <$> bigM p <*> bigM q

bigM p @ (Alt _ _) = do

  (ys, con) <- fmap unzip $ forM (disjunctions p) $ \ this -> do
    (y, z) <- binary
    pure (y, Con (Equal (y + z) 1) <> Con (withM z this))

  pure $ conjunction $ mconcat con <> Con (Equal (sum ys) 1)

bigM p = pure p


findM :: LP Integer
findM = do

  Program _ p bounds <- sProgram <$> lps

  let inferMax = foldl' max 0 $ maxBoundFromSubjectTo <$> conjunctions p
      boundMax = foldl' (\ a (Bound _ b _) -> max a b) 0 bounds

  pure $ succ $ maximum [2, boundMax, inferMax]


maxBoundFromSubjectTo :: SubjectTo -> Integer
maxBoundFromSubjectTo (LessEq _ (Lit n)) = n
maxBoundFromSubjectTo (Equal  _ (Lit n)) = n
maxBoundFromSubjectTo _ = 0


disjunctions :: SubjectTo -> [SubjectTo]
disjunctions (Alt a b) = disjunctions a ++ disjunctions b
disjunctions a = [a]

conjunctions :: SubjectTo -> [SubjectTo]
conjunctions (Cont a b) = conjunctions a ++ conjunctions b
conjunctions a = [a]


withM :: Var -> SubjectTo -> SubjectTo
withM z (Cont  a b) = Cont (withM z a) (withM z b)
withM z (GreaterEq a b) = GreaterEq (a + M * z) b
withM z    (LessEq a b) =    LessEq (a - M * z) b
withM z     (Equal a b) = withM z $ Cont (GreaterEq a b) (LessEq a b)
withM _ st = st




data Program = Program Objective SubjectTo [Bound]
  deriving (Eq, Show)

instance Semigroup Program where
  Program o st bs <> Program p tt cs
    = Program (o <> p) (conjunction $ Con st <> Con tt) (bs <> cs)

instance Monoid Program where
  mempty = Program mempty (conjunction mempty) mempty
  mappend = (<>)


newtype Objective = Objective Exp
  deriving (Eq, Show)

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
  deriving (Eq, Show)


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


type Var = Exp

data Bound = Bound Integer Integer Var
  deriving (Eq, Show)


data Exp
  = Sym Int
  | Bin Int | Bin' Int
  | M
  | Lit Integer
  | Neg Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Sub Exp Exp
  deriving (Eq, Generic, Show)

instance Hashable Exp


instance Num Exp where

  Lit a + Lit b = Lit (a + b)
  Lit 0 + a = a
  a + Lit 0 = a
  a + b = Add a b

  Lit a - Lit b = Lit (a - b)
  Lit 0 - Neg a = a
  Lit 0 - a = Neg a
  a - Lit 0 = a
  a - b = Sub a b

  Lit a * Lit b = Lit (a * b)
  Lit 0 * _ = Lit 0
  _ * Lit 0 = Lit 0
  Lit 1 * a = a
  a * Lit 1 = a
  Neg a * Neg b = a * b
  a * b = Mul a b

  fromInteger = literal

  abs (Neg (Neg e)) = abs e
  abs (Neg e) = e
  abs (Lit n) = literal (abs n)
  abs e = e

  signum (Neg (Neg e)) = signum e
  signum (Neg _) = literal (-1)
  signum (Lit n) = literal (signum n)
  signum e = e


infix  4 <=.
infixr 3 .<=

(<=.) :: Integer -> Var -> Integer -> LP ()
(<=.) l v u = bound l u v

(.<=) :: (Integer -> LP ()) -> Integer -> LP ()
(.<=) = ($)


infix 4 <=^, >=^, =^

(=^), (<=^), (>=^) :: Monad m => Exp -> Exp -> LPT m ()

a =^ b | isPrim b = subjectTo $ Equal a b
_ =^ _ = fail "right-hand side expression not supported"

a <=^ b | isPrim b = subjectTo $ LessEq a b
_ <=^ _ = fail "right-hand side expression not supported"

a >=^ b | isPrim b = subjectTo $ GreaterEq a b
_ >=^ _ = fail "right-hand side expression not supported"


isPrim :: Exp -> Bool
isPrim (Sym _) = True
isPrim (Bin _) = True
isPrim (Lit _) = True
isPrim M = True
isPrim _ = False



type Result = Var -> Maybe Integer


data LPS = LPS
  { xTicket  :: !Int
  , yTicket  :: !Int
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

  empty = LP $ error "empty lp" <$ put def
    where def = LPS 0 0 (Program mempty (disjunction mempty) mempty) (const Nothing)

  f <|> g = do

    Program o s a <- sProgram <$> lps <* truncate
    x <- f

    Program _ t b <- sProgram <$> lps <* truncate
    y <- g

    Program _ u c <- sProgram <$> lps

    let q = disjunction $ Dis t <> Dis u
        p = conjunction $ Con s <> Con q

    up $ Program o p (a <> b <> c)

    case (t, u) of
      (Zero, Zero) -> pure $ error "empty lp"
      (Zero, _) -> pure y
      _ -> pure x


instance Monad m => MonadPlus (LPT m) where
  mzero = empty
  mplus = (<|>)



runLP :: LP a -> a
runLP = fst . runIdentity . runLPT (LPS 0 0 mempty (const Nothing))

runLPT :: Monad m => LPS -> LPT m a -> m (a, LPS)
runLPT s m = runStateT (unLP m) s



lps :: Monad m => LPT m LPS
lps = LP get

up :: Monad m => Program -> LPT m ()
up p = LP $ do
  s <- get
  put s { sProgram = p }


optimize :: Monad m => LPT m ()
optimize = do
  Program _ p _ <- sProgram <$> lps
  q <- convexHull p
  Program o _ b <- sProgram <$> lps
  up $ Program o q b


literal :: Integer -> Exp
literal = Lit

general :: Monad m => LPT m Var
general = do
  ticket <- succ . xTicket <$> lps
  LP $ modify $ \ s -> s { xTicket = ticket }
  pure $ Sym ticket

binary :: Monad m => LPT m (Var, Var)
binary = do
  ticket <- succ . yTicket <$> lps
  let (y, z) = (Bin ticket, Bin' ticket)
  y + z =^ 1
  LP $ modify $ \ s -> s { yTicket = ticket }
  pure (y, z)


truncate :: Monad m => LPT m ()
truncate = up mempty

prog :: Monad m => Program -> LPT m ()
prog q = do
  p <- sProgram <$> lps
  up $ p <> q


objective :: Monad m => Exp -> LPT m ()
objective e = prog $ Program (Objective e) (conjunction mempty) mempty


subjectTo :: Monad m => SubjectTo -> LPT m ()
subjectTo s = prog $ Program mempty s mempty

bound :: Monad m => Integer -> Integer -> Var -> LPT m ()
bound a b x = prog $ Program mempty (conjunction mempty) [Bound a b x]

