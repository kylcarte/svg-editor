{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Expr where

import Data.String (IsString(..))
import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map String

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

eval :: Floating a => Env a -> Expr -> Either EvalErr a
eval env = evalWith (evalVar env) env

subst :: Env Expr -> Expr -> Expr
subst env e = case evalWith (return . Var) env e of
  Right e' -> e'
  Left err -> error $ "Unexpected error in subst: " ++ show err

evalVar :: Env a -> String -> Either EvalErr a
evalVar env x =
  maybe (Left $ UnboundVar x) return
  $ Map.lookup x env

evalWith :: Floating a => (String -> Either EvalErr a) -> Env a -> Expr -> Either EvalErr a
evalWith unb env = \case
  Val v ->
    return $ realToFrac v
  Var x ->
    maybe (unb x) return
    $ Map.lookup x env
  Add e1 e2 ->
    (+) <$> evalWith unb env e1 <*> evalWith unb env e2
  Mul e1 e2 ->
    (*) <$> evalWith unb env e1 <*> evalWith unb env e2
  Neg e ->
    negate <$> evalWith unb env e
  Recip e ->
    recip <$> evalWith unb env e
  Abs e ->
    abs <$> evalWith unb env e
  Signum e ->
    signum <$> evalWith unb env e
  Exp e ->
    exp <$> evalWith unb env e
  Log e ->
    log <$> evalWith unb env e
  Sqrt e ->
    sqrt <$> evalWith unb env e
  Pow e1 e2 ->
    (**) <$> evalWith unb env e1 <*> evalWith unb env e2
  Sin e ->
    sin <$> evalWith unb env e
  Cos e ->
    cos <$> evalWith unb env e
  Asin e ->
    asin <$> evalWith unb env e
  Acos e ->
    acos <$> evalWith unb env e
  Atan e ->
    atan <$> evalWith unb env e
  Sinh e ->
    sinh <$> evalWith unb env e
  Cosh e ->
    cosh <$> evalWith unb env e
  Asinh e ->
    asinh <$> evalWith unb env e
  Acosh e ->
    acosh <$> evalWith unb env e
  Atanh e ->
    atanh <$> evalWith unb env e

-- }}}

