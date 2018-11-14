{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Expr where

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
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

evalVar ::
  ( Member (Error EvalErr) sig
  , Carrier sig m
  , Monad m
  ) => Env a -> String -> m a
evalVar env x = throwUnlessJust (UnboundVar x) $ Map.lookup x env

eval ::
  ( Floating a
  , Member (Error EvalErr) sig
  , Carrier sig m
  , Monad m
  ) => Env a -> Expr -> m a
eval env = \case
  Val v ->
    return $ realToFrac v
  Var x ->
    evalVar env x
  Add e1 e2 ->
    (+) <$> eval env e1 <*> eval env e2
  Mul e1 e2 ->
    (*) <$> eval env e1 <*> eval env e2
  Neg e ->
    negate <$> eval env e
  Recip e ->
    recip <$> eval env e
  Abs e ->
    abs <$> eval env e
  Signum e ->
    signum <$> eval env e
  Exp e ->
    exp <$> eval env e
  Log e ->
    log <$> eval env e
  Sqrt e ->
    sqrt <$> eval env e
  Pow e1 e2 ->
    (**) <$> eval env e1 <*> eval env e2
  Sin e ->
    sin <$> eval env e
  Cos e ->
    cos <$> eval env e
  Asin e ->
    asin <$> eval env e
  Acos e ->
    acos <$> eval env e
  Atan e ->
    atan <$> eval env e
  Sinh e ->
    sinh <$> eval env e
  Cosh e ->
    cosh <$> eval env e
  Asinh e ->
    asinh <$> eval env e
  Acosh e ->
    acosh <$> eval env e
  Atanh e ->
    atanh <$> eval env e

throwUnlessJust ::
  ( Member (Error e) sig
  , Carrier sig m
  , Monad m
  ) => e -> Maybe a -> m a
throwUnlessJust e = maybe (throwError e) return

-- }}}

