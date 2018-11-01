{-# LANGUAGE LambdaCase #-}

module Expr where

import Data.String (IsString(..))
import Data.Map (Map)
import qualified Data.Map as Map

data Expr
  = Val    Double
  | Var    String
  | Add    Expr Expr
  | Mul    Expr Expr
  | Neg    Expr
  | Recip  Expr
  | Abs    Expr
  | Signum Expr
  | Exp    Expr
  | Log    Expr
  | Sqrt   Expr
  | Pow    Expr Expr
  | Sin    Expr
  | Cos    Expr
  | Asin   Expr
  | Acos   Expr
  | Atan   Expr
  | Sinh   Expr
  | Cosh   Expr
  | Asinh  Expr
  | Acosh  Expr
  | Atanh  Expr
  deriving (Eq,Ord,Show)

-- instances {{{

instance Num Expr where
  (+)         = valFn2 (+) Add
  (*)         = valFn2 (*) Mul
  negate      = valFn1 negate Neg
  abs         = valFn1 abs Abs
  signum      = valFn1 signum Signum
  fromInteger = Val . fromInteger

instance Fractional Expr where
  recip        = valFn1 recip Recip
  fromRational = Val . fromRational

instance Floating Expr where
  pi    = Val pi
  exp   = valFn1 exp   Exp
  log   = valFn1 log   Log
  sqrt  = valFn1 sqrt  Sqrt
  (**)  = valFn2 (**)  Pow
  sin   = valFn1 sin   Sin
  cos   = valFn1 cos   Cos
  asin  = valFn1 asin  Asin
  acos  = valFn1 acos  Acos
  atan  = valFn1 atan  Atan
  sinh  = valFn1 sinh  Sinh
  cosh  = valFn1 cosh  Cosh
  asinh = valFn1 asinh Asinh
  acosh = valFn1 acosh Acosh
  atanh = valFn1 atanh Atanh

instance IsString Expr where
  fromString = Var

valFn1 :: (Double -> Double)
       -> (Expr -> Expr)
       -> Expr -> Expr
valFn1 fv fe = \case
  Val a -> Val $ fv a
  a     -> fe a

valFn2 :: (Double -> Double -> Double)
       -> (Expr -> Expr -> Expr)
       -> Expr -> Expr -> Expr
valFn2 fv fe = \case
  Val a -> \case
    Val b -> Val $ fv a b
    b     -> fe (Val a) b
  a     -> fe a

-- }}}

-- evaluation {{{

data EvalErr
  = UnboundVar String
  deriving (Eq,Ord,Show)

type Env = Map String

newtype Eval a b = Eval
  { unEval :: Env a -> Either EvalErr b
  }

instance Functor (Eval a) where
  fmap f = Eval . fmap (fmap f) . unEval

instance Applicative (Eval a) where
  pure = Eval . pure . pure
  Eval f <*> Eval x = Eval $ \env ->
    f env <*> x env

instance Monad (Eval a) where
  Eval m >>= f = Eval $ \env ->
    let r = m env
    in r >>= ($ env) . unEval . f

curEnv :: Eval a (Env a)
curEnv = Eval return

evalErr :: EvalErr -> Eval a b
evalErr = Eval . const . Left

evalVar :: String -> Eval a a
evalVar x = do
  env <- curEnv
  maybe (evalErr $ UnboundVar x) return
    $ Map.lookup x env

eval :: Floating a => Expr -> Eval a a
eval = \case
  Val v ->
    return $ fromRational $ toRational v
  Var x ->
    evalVar x
  Add e1 e2 ->
    (+) <$> eval e1 <*> eval e2
  Mul e1 e2 ->
    (*) <$> eval e1 <*> eval e2
  Neg e ->
    negate <$> eval e
  Recip e ->
    recip <$> eval e
  Abs e ->
    abs <$> eval e
  Signum e ->
    signum <$> eval e
  Exp e ->
    exp <$> eval e
  Log e ->
    log <$> eval e
  Sqrt e ->
    sqrt <$> eval e
  Pow e1 e2 ->
    (**) <$> eval e1 <*> eval e2
  Sin e ->
    sin <$> eval e
  Cos e ->
    cos <$> eval e
  Asin e ->
    asin <$> eval e
  Acos e ->
    acos <$> eval e
  Atan e ->
    atan <$> eval e
  Sinh e ->
    sinh <$> eval e
  Cosh e ->
    cosh <$> eval e
  Asinh e ->
    asinh <$> eval e
  Acosh e ->
    acosh <$> eval e
  Atanh e ->
    atanh <$> eval e

-- }}}

