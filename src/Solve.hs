{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Solve where

import Doc
import Expr
import Spec
import Numeric.AD.Newton hiding (eval)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- {{{

newtype Fn a = Fn
  { with :: Env a -> a
  }

instance Num a => Num (Fn a) where
  (+) = fn2 (+)
  (*) = fn2 (*)
  (-) = fn2 (-)
  abs = fn1 abs
  signum = fn1 signum
  fromInteger = fn0 . fromInteger

instance Fractional a => Fractional (Fn a) where
  (/)          = fn2 (/)
  fromRational = fn0 . fromRational

instance Floating a => Floating (Fn a) where
  pi    = fn0 pi
  exp   = fn1 exp
  log   = fn1 log
  sin   = fn1 sin
  cos   = fn1 cos
  asin  = fn1 asin
  acos  = fn1 acos
  atan  = fn1 atan
  sinh  = fn1 sinh
  cosh  = fn1 cosh
  asinh = fn1 asinh
  acosh = fn1 acosh
  atanh = fn1 atanh

fn0 :: a -> Fn a
fn0 = Fn . const

fn1 :: (a -> a) -> Fn a -> Fn a
fn1 f x = Fn $ \env -> f (x `with` env)

fn2 :: (a -> a -> a) -> Fn a -> Fn a -> Fn a
fn2 f x y = Fn $ \env -> f (x `with` env) (y `with` env)

var :: String -> Fn a
var x = Fn (Map.! x)

dbl :: Fractional a => Double -> Fn a
dbl = fn0 . realToFrac

check :: Floating a => Env b -> Expr -> Either EvalErr (Fn a)
check vars = \case
  Val v ->
    return $ fn0 $ realToFrac v
  Var x -> if x `Map.member` vars
    then return $ var x
    else Left $ UnboundVar x
  Add e1 e2 ->
    (+) <$> check vars e1 <*> check vars e2
  Mul e1 e2 ->
    (*) <$> check vars e1 <*> check vars e2
  Neg e ->
    negate <$> check vars e
  Recip e ->
    recip <$> check vars e
  Abs e ->
    abs <$> check vars e
  Signum e ->
    signum <$> check vars e
  Exp e ->
    exp <$> check vars e
  Log e ->
    log <$> check vars e
  Sqrt e ->
    sqrt <$> check vars e
  Pow e1 e2 ->
    (**) <$> check vars e1 <*> check vars e2
  Sin e ->
    sin <$> check vars e
  Cos e ->
    cos <$> check vars e
  Asin e ->
    asin <$> check vars e
  Acos e ->
    acos <$> check vars e
  Atan e ->
    atan <$> check vars e
  Sinh e ->
    sinh <$> check vars e
  Cosh e ->
    cosh <$> check vars e
  Asinh e ->
    asinh <$> check vars e
  Acosh e ->
    acosh <$> check vars e
  Atanh e ->
    atanh <$> check vars e

-- }}}

mkCostFunction :: Floating a => Env b -> (Expr,Expr) -> Either EvalErr ((a,a) -> Fn a)
mkCostFunction vars (ex,ey) = do
  fx <- check vars ex
  fy <- check vars ey
  return $ \(x,y) -> euclDist (fx,fy) (fn0 x,fn0 y)

mkEqConstraint :: Floating a => Env b -> Env a -> Expr -> Either EvalErr (Fn a,a)
mkEqConstraint vars env e = (,)
  <$> check vars e
  <*> eval env e

checkDefs :: Floating a => ShapeType -> Either EvalErr (Env (Fn a))
checkDefs st = traverse (check $ shapeParams st) $ shapeDefs st

moveControl :: Floating a => ShapeType -> ShapeVal -> String -> Either EvalErr ((a,a) -> ConOptProblem a)
moveControl st sv h = do
  (hex, hey) <- evalVar (shapeHandles st) h
  let defs = shapeDefs st
  let fes = handleFixList st h
  cfn  <- mkCostFunction
          (shapeParams st)
          (subst defs hex,subst defs hey)
  eqcs <- mapM ( mkEqConstraint
                 (shapeParams st)
               $ realToFrac <$> shapeVal sv
               ) fes
  return $ \(x,y) -> ConOptProblem
    { costFunction    = cfn (x,y)
    , eqConstraints   = eqcs
    , ineqConstraints = []
    }

data ConOptProblem a = ConOptProblem
  { costFunction    :: Fn a
  , eqConstraints   :: [(Fn a, a)]
  , ineqConstraints :: [(Fn a, a)]
  }

euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

