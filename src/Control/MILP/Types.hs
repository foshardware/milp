{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveGeneric #-}

module Control.MILP.Types where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.Morph
import Control.Monad.State hiding (fail)
import Data.Functor.Identity
import Data.Foldable

import Data.Map (Map, insertWith, foldrWithKey)
import Data.Hashable
import Data.Text (Text)

import GHC.Generics

import Prelude hiding (fail, truncate)


convexHull :: Monad m => SubjectTo -> LPT m SubjectTo

convexHull (Cont p q) = Cont <$> convexHull p <*> convexHull q

convexHull p@(Alt _ _) = bigM p

convexHull p = pure p


bigM :: Monad m => SubjectTo -> LPT m SubjectTo

bigM (Cont p q) = Cont <$> bigM p <*> bigM q

bigM p@(Alt _ _) = do

  bins <- sequence $ binary <$ disjunctions p

  form <- pure
    [ C (Eq (y + z) 1) <> C (withM z q)
    | q <- disjunctions p
    | (y, z) <- bins
    ]

  let exclusiveOr = Eq (sum $ fst <$> bins) 1

  pure $ conjunction $ C exclusiveOr <> mconcat form

bigM p = pure p


findM :: Monad m => LPT m Integer
findM = do

  Program _ p bounds _ _ <- sProgram <$> lps

  let inferMax = foldl' max 0 $ maxBoundFromSubjectTo <$> conjunctions p
      boundMax = foldl' (\ a (Bound _ b _) -> max a b) 0 bounds

  pure $ succ $ maximum [2, boundMax, inferMax]


maxBoundFromSubjectTo :: SubjectTo -> Integer
maxBoundFromSubjectTo (LtEq _ (Lit n)) = n
maxBoundFromSubjectTo   (Eq _ (Lit n)) = n
maxBoundFromSubjectTo _ = 0


disjunctions :: SubjectTo -> [SubjectTo]
disjunctions (Alt a b) = disjunctions a ++ disjunctions b
disjunctions a = [a]

conjunctions :: SubjectTo -> [SubjectTo]
conjunctions (Cont a b) = conjunctions a ++ conjunctions b
conjunctions a = [a]


withM :: Var -> SubjectTo -> SubjectTo
withM z (Cont a b) = Cont (withM z a) (withM z b)
withM z (GtEq a b) = GtEq (a + M * z) b
withM z (LtEq a b) = LtEq (a - M * z) b
withM z   (Eq a b) = withM z $ Cont (GtEq a b) (LtEq a b)
withM _ p = p



data Program = Program Objective SubjectTo [Bound] [Var] [Var]
  deriving (Eq, Show)

instance Semigroup Program where
  Program o st bs gens1 bins1 <> Program p tt cs gens2 bins2
    = Program (o <> p) (conjunction $ C st <> C tt) (bs <> cs) (gens1 <> gens2) (bins1 <> bins2)

instance Monoid Program where
  mempty = Program mempty (conjunction mempty) mempty mempty mempty
  mappend = (<>)


newtype Objective = Objective Exp
  deriving (Eq, Show)

instance Semigroup Objective where
  Objective a <> Objective b = Objective (a + b)

instance Monoid Objective where
  mempty = Objective 0
  mappend = (<>)


data SubjectTo
  = Eq Exp Exp
  | LtEq Exp Exp
  | GtEq Exp Exp
  | Unit
  | Cont SubjectTo SubjectTo
  | Zero
  | Alt SubjectTo SubjectTo
  deriving (Eq, Show)


newtype Disjunction = D { disjunction :: SubjectTo }

instance Semigroup Disjunction where
  D Zero <> a = a
  a <> D Zero = a
  D a <> D b = D (Alt a b)

instance Monoid Disjunction where
  mempty = D Zero
  mappend = (<>)


newtype Conjunction = C { conjunction :: SubjectTo }

instance Semigroup Conjunction where
  C Unit <> a = a
  a <> C Unit = a
  C a <> C b = C (Cont a b)

instance Monoid Conjunction where
  mempty = C Unit
  mappend = (<>)


type Var = Exp

data Bound = Bound Integer Integer Var
  deriving (Eq, Show)


data Exp
  = Sym Int
  | Named Text
  | Bin Int | Bin' Int
  | M
  | Lit Integer
  | Float Double
  | Inf
  | Neg Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Sub Exp Exp
  deriving (Eq, Ord, Generic, Show)

instance Hashable Exp


instance Num Exp where

  Lit a + Lit b = Lit (a + b)
  Lit 0 + a = a
  a + Lit 0 = a
  Neg a + b = b - a
  a + Neg b = a - b
  a + Lit n | n < 0 = a - literal (abs n)
  Lit n + a | n < 0 = a - literal (abs n)
  a + b = Add a b

  Lit a - Lit b = Lit (a - b)
  Lit 0 - Neg a = a
  Lit 0 - a = Neg a
  a - Lit 0 = a
  a - Neg b = a + b
  a - Lit n | n < 0 = a + literal (abs n)
  a - b = Sub a b

  Lit a * Lit b = Lit (a * b)
  Lit 0 * _ = Lit 0
  _ * Lit 0 = Lit 0
  Lit 1 * a = a
  a * Lit 1 = a
  Lit (-1) * Neg a = a
  Neg a * Lit (-1) = a
  Lit (-1) * a = Neg a
  a * Lit (-1) = Neg a
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

a =^ b
  = subjectTo
  $ (\ (c, d) -> Eq (remove c) (d - offset c))
  $ normalSubjectTo (sortExp a, sortExp b)

a <=^ b
  = subjectTo
  $ (\ (c, d) -> LtEq (remove c) (d - offset c))
  $ normalSubjectTo (sortExp a, sortExp b)

a >=^ b
  = subjectTo
  $ (\ (c, d) -> GtEq (remove c) (d - offset c))
  $ normalSubjectTo (sortExp a, sortExp b)


remove :: Exp -> Exp
remove (Lit _) = 0
remove (Add a b) = remove a + remove b
remove (Sub a b) = remove a - remove b
remove e = e

offset :: Exp -> Exp
offset (Lit n) = literal n
offset (Add a b) = offset a + offset b
offset (Sub a b) = offset a - offset b
offset _ = 0


sortExp :: Exp -> Exp
sortExp e = foldrWithKey
  (\ k n a -> literal n * k + a) 0
  (normalize e `execState` mempty)


normalSubjectTo :: (Exp, Exp) -> (Exp, Exp)
normalSubjectTo (a,   Lit n) = (sortExp a, Lit n)
normalSubjectTo (a,   Sym x) = normalSubjectTo (a - Sym x, 0)
normalSubjectTo (a,  Bin  y) = normalSubjectTo (a - Bin  y, 0)
normalSubjectTo (a,  Bin' y) = normalSubjectTo (a - Bin' y, 0)
normalSubjectTo (a, Sub b c) = normalSubjectTo (a + c, b)
normalSubjectTo (a, Add b c) = normalSubjectTo (a - b, c)
normalSubjectTo (a, b) = (sortExp a, sortExp b)


normalize :: Exp -> State (Map Exp Integer) ()
normalize (Lit n) = modify $ insertWith (+) (Lit 1) n
normalize (Sym x) = modify $ insertWith (+) (Sym x) 1
normalize (Bin  y) = modify $ insertWith (+) (Bin  y) 1
normalize (Bin' y) = modify $ insertWith (+) (Bin' y) 1
normalize (Neg (Neg x)) = normalize x
normalize (Neg (Sym x)) = modify $ insertWith (+) (Sym x) (-1)
normalize (Neg (Lit x)) = modify $ insertWith (+) (Lit 1) (-x)
normalize (Neg (Bin  y)) = modify $ insertWith (+) (Bin  y) (-1)
normalize (Neg (Bin' y)) = modify $ insertWith (+) (Bin' y) (-1)
normalize (Mul (Lit n) (Sym x)) = modify $ insertWith (+) (Sym x) n
normalize (Mul (Lit n) (Bin  y)) = modify $ insertWith (+) (Bin  y) n
normalize (Mul (Lit n) (Bin' y)) = modify $ insertWith (+) (Bin' y) n
normalize (Add a b) = normalize a *> normalize b
normalize (Sub a b) = normalize a *> normalize (Neg b)
normalize _ = pure ()



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

instance MonadIO m => MonadIO (LPT m) where
  liftIO = LP . liftIO

instance Monad m => MonadFail (LPT m) where
  fail = error

instance Monad m => Alternative (LPT m) where

  empty = error "empty lp" <$ up (Program mempty Zero mempty mempty mempty)

  f <|> g = do

    Program o s a gens1 bins1 <- sProgram <$> lps <* truncate
    x <- f

    Program _ t b gens2 bins2 <- sProgram <$> lps <* truncate
    y <- g

    Program _ u c gens3 bins3 <- sProgram <$> lps

    let q = disjunction $ D t <> D u
        p = conjunction $ C s <> C q

    up $ Program o p (a <> b <> c) (gens1 <> gens2 <> gens3) (bins1 <> bins2 <> bins3)

    case t of
      Zero -> pure y
      _    -> pure x


instance Monad m => MonadPlus (LPT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LPT where
  lift = LP . lift

instance MFunctor LPT where
  hoist nat = LP . hoist nat . unLP


start :: LPS
start = LPS 0 0 mempty (const Nothing)

runLP :: Monad m => LPS -> LPT m a -> m (a, LPS)
runLP s m = runStateT (unLP m) s

evalLP :: Monad m => LPS -> LPT m a -> m a
evalLP s = fmap fst . runLP s


lps :: Monad m => LPT m LPS
lps = LP get

up :: Monad m => Program -> LPT m ()
up p = LP $ do
  s <- get
  put s { sProgram = p }


optimize :: Monad m => LPT m ()
optimize = do
  Program _ p _ gens bins <- sProgram <$> lps
  q <- convexHull p
  Program o _ b gens bins <- sProgram <$> lps
  up $ Program o q b gens bins


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
objective e = prog $ Program (Objective e) (conjunction mempty) mempty mempty mempty


subjectTo :: Monad m => SubjectTo -> LPT m ()
subjectTo s = prog $ Program mempty s mempty mempty mempty

bound :: Monad m => Integer -> Integer -> Var -> LPT m ()
bound a b x@(Sym _) = prog $ Program mempty (conjunction mempty) [Bound a b x] mempty mempty
bound _ _ _ = fail "bound on not primitive"


vars :: Exp -> [Var]
vars (Add a b) = vars a ++ vars b
vars (Sub a b) = vars a ++ vars b
vars (Sym x) = [Sym x]
vars _ = []

