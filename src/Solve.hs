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

import Control.Effect
import Control.Effect.Error
import Control.Effect.Reader
import Data.Map (Map)
import qualified Data.Map as Map

moveControl ::
  ( Member (Error EvalErr) sig
  , Carrier sig m
  , Monad m
  , Floating a
  ) => ShapeType -> ShapeVal -> String -> (a,a) -> m (ConOptProblem a)
moveControl st sv h (x,y) = do
  (hx, hy) <- evalVar (shapeHandles st) h
  let binds = shapeBindings st
  let fes   = handleFixList st h
  ex <- eval binds hx
  ey <- eval binds hy
  es <- mapM (eval binds) fes
  _

{-
TODO: write out concrete examples of input/output
ShapeType, ShapeVal, handle name -> con opt problem

tests for each handles...
  * behavior in overconstrained systems?
  * rotation by a little bit
  * handworkable 

-}

data ConOptProblem a = ConOptProblem
  { costFunction    :: Env a -> a
  , eqConstraints   :: [(Env a -> a, a)]
  -- , ineqConstraints :: [(Env a -> a, a)]
  }

euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

