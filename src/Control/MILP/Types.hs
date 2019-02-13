{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Control.MILP.Types where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.State hiding (fail)
import Data.Functor.Identity

import Prelude hiding (fail, truncate)


constantM :: Integer
constantM = 1000


bigM :: Monad m => SubjectTo -> LPT m SubjectTo

bigM (Cont st tt) = Cont <$> bigM st <*> bigM tt

bigM st@(Alt _ _) = do

  (ys, con) <- unzip <$> sequence
    [ do
      (y, z) <- binary
      let con = Con (Equal (y + z) 1) <> Con (withM z dis)
      pure (y, con)
    | dis <- disjunctions st
    ]

  pure $ conjunction $ mconcat con <> Con (Equal (sum ys) 1)


bigM st = pure st


findM :: LP Integer
findM = pure 1000


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
  deriving (Eq, Ord, Show)


instance Num Exp where

  Lit 0 + a = a
  a + Lit 0 = a
  a + b = Add a b

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

    i <- lps

    s <- sProgram <$> lps <* truncate
    x <- f

    t <- sProgram <$> lps <* truncate
    y <- g

    u <- sProgram <$> lps

    let Program o st a = s
        Program _ tt b = t
        Program _ ut c = u

    let q = disjunction $ Dis tt <> Dis ut
        p = conjunction $ Con st <> Con q

    LP $ put i { sProgram = Program o p (a <> b <> c) }

    case (tt, ut) of
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
  Program _ st _ <- sProgram <$> lps
  subj <- bigM st
  Program o _ bs <- sProgram <$> lps
  up $ Program o subj bs


literal :: Integer -> Exp
literal = Lit

general :: Monad m => LPT m Var
general = do
  x <- xTicket <$> lps
  LP $ modify $ \ s -> s { xTicket = succ x }
  pure $ Sym $ succ x

binary :: Monad m => LPT m (Var, Var)
binary = do
  t <- yTicket <$> lps
  let (y, z) = (Bin $ succ t, Bin' $ succ t)
  y + z =^ 1
  LP $ modify $ \ s -> s { yTicket = succ t }
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

