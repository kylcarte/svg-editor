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
import Numeric.AD.Mode.Reverse (Reverse,auto)
import Numeric.AD.Internal.Reverse (Tape)
import Data.Reflection (Reifies)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- our own CC data type, to avoid orphan instances
data Fn a = Fn
  { apFn :: forall s. Reifies s Tape => Env (Reverse s a) -> Reverse s a
  }

instance Num a => Num (Fn a) where
  (+) = fn2 (+)
  (*) = fn2 (*)
  (-) = fn2 (-)
  abs = fn1 abs
  signum = fn1 signum
  fromInteger i = fn0 $ fromInteger i

instance Fractional a => Fractional (Fn a) where
  (/)          = fn2 (/)
  fromRational r = fn0 $ fromRational r

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

fn0 :: (forall s. Reifies s Tape => Reverse s a)
    -> Fn a
fn0 f = Fn $ \_ -> f

fn1 :: (forall s. Reifies s Tape => Reverse s a -> Reverse s a)
    -> Fn a -> Fn a
fn1 f (Fn x) = Fn $ \env -> f (x env)

fn2 :: (forall s. Reifies s Tape => Reverse s a -> Reverse s a -> Reverse s a)
    -> Fn a -> Fn a -> Fn a
fn2 f (Fn x) (Fn y) = Fn $ \env -> f (x env) (y env)

var :: String -> Fn a
var x = Fn $ (Map.! x)

dbl :: Fractional a => Double -> Fn a
dbl n = fn0 $ auto $ realToFrac n

check :: Floating a => Env b -> Expr -> Either EvalErr (Fn a)
check params = \case
  Val v ->
    return $ dbl v
  Var x -> if x `Map.member` params
    then return $ var x
    else Left $ UnboundVar x
  Add e1 e2 ->
    (+) <$> check params e1 <*> check params e2
  Mul e1 e2 ->
    (*) <$> check params e1 <*> check params e2
  Neg e ->
    negate <$> check params e
  Recip e ->
    recip <$> check params e
  Abs e ->
    abs <$> check params e
  Signum e ->
    signum <$> check params e
  Exp e ->
    exp <$> check params e
  Log e ->
    log <$> check params e
  Sqrt e ->
    sqrt <$> check params e
  Pow e1 e2 ->
    (**) <$> check params e1 <*> check params e2
  Sin e ->
    sin <$> check params e
  Cos e ->
    cos <$> check params e
  Asin e ->
    asin <$> check params e
  Acos e ->
    acos <$> check params e
  Atan e ->
    atan <$> check params e
  Sinh e ->
    sinh <$> check params e
  Cosh e ->
    cosh <$> check params e
  Asinh e ->
    asinh <$> check params e
  Acosh e ->
    acosh <$> check params e
  Atanh e ->
    atanh <$> check params e

mkCostFunction :: Floating a => Env b -> (Expr,Expr) -> Either EvalErr ((Double,Double) -> Fn a)
mkCostFunction params (ex,ey) = do
  fx <- check params ex
  fy <- check params ey
  return $ \(x,y) -> euclDist (fx,fy) (dbl x,dbl y)

mkEqConstraint :: Floating a => Env b -> Env Expr -> Env a -> Expr -> Either EvalErr (Fn a,a)
mkEqConstraint params defs env e = (,)
  <$> check params e'
  <*> eval env e'
  where
  e' = subst defs e

checkDefs :: Floating a => ShapeType -> Either EvalErr (Env (Fn a))
checkDefs st = traverse (check $ shapeParams st) $ shapeDefs st

moveControl :: Floating a => ShapeType -> String -> ShapeVal -> Either EvalErr ((Double,Double) -> ConOptProblem a)
moveControl st h sv = do
  (hex, hey) <- evalVar (shapeHandles st) h
  let defs = shapeDefs st
  let fes = handleFixList st h
  let params = shapeParams st
  cfn <- mkCostFunction
          params
          (subst defs hex,subst defs hey)
  let env = realToFrac <$> shapeVal sv
  eqcs <- mapM (mkEqConstraint params defs env) fes
  return $ \hMoved -> ConOptProblem
    { costFunction    = cfn hMoved
    , eqConstraints   = eqcs
    }

data ConOptProblem a = ConOptProblem
  { costFunction    :: Fn a
  , eqConstraints   :: [(Fn a, a)]
  }

euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

type Lagrangian = Map (Either Int String)

toLagrangian :: Num a => ConOptProblem a -> CC Lagrangian a
toLagrangian p = CC $ \env ->
  case costFunction p of
    Fn f ->
      let base_env = rightKeys env
      in foldr
      ( \(i,(Fn g,c)) ->
        let l_mul = env Map.! Left i
        in subtract
        $ l_mul * (g base_env + auto c)
      )
      (f base_env)
      $ zip [0..]
      $ eqConstraints p

rightKeys :: Ord k => Map (Either k' k) a -> Map k a
rightKeys = Map.foldMapWithKey $ either (\_ _ -> mempty) Map.singleton

solveProblem :: ShapeVal -> ConOptProblem Double -> [Env Double]
solveProblem sv p = case toLagrangian p of
  CC f -> fmap rightKeys $ gradientDescent f $ multipliers <> Map.mapKeys Right (shapeVal sv)
  where
  m = length $ eqConstraints p
  multipliers = Map.fromList
    [ (Left i, 1)
    | i <- [0 .. m - 1]
    ]

allTogetherNow :: ShapeType -> String -> ShapeVal -> Either EvalErr ((Double,Double) -> [Env Double])
allTogetherNow st h sv = fmap (solveProblem sv) <$> moveControl st h sv

