{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Problem where

import Spec
import Expr
import Opt

import Control.Arrow ((***))
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: control where typechecking happens, make evaluating safe

moveHandle :: ShapeType -> Env Float -> Handle -> (Float,Float) -> Env Float -- (Env Float,String)
moveHandle st env h p =
  runOpt (initParams objFn) env
  where
  objFn :: AugObjFn
  objFn c env' =
    either (error . ("moveHandle: " ++) . show) id $ do
      hpos <- evalHandle st env' h
      fvs <- evalFixList st env h
      pps <- traverse
             ( \(e,(realToFrac -> fv)) -> do
               v <- evalWithShape st env' e
               return (v - fv, fv - v)
             ) fvs
      let (ps1,ps2) = unzip pps
      return $
        distsq (abstractPt p) hpos
        + penalties c (ps1 ++ ps2)
    where
    fixed = handleFixList st h

evalFixList :: Floating a => ShapeType -> Env a -> Handle -> Either EvalErr [(Expr,a)]
evalFixList st env h =
  traverse
  ( \e -> (,) e <$> evalWithShape st env e
  ) $ handleFixList st h

evalHandle :: Floating a => ShapeType -> Env a -> Handle -> Either EvalErr (a,a)
evalHandle st env h = do
  (ex,ey) <- maybe (Left $ UnboundVar h) Right
             $ Map.lookup h
             $ shapeHandles st
  (,) <$> ev ex <*> ev ey
  where
  ev = evalWithShape st env

abstractPt :: Fractional a => (Float,Float) -> (a,a)
abstractPt = realToFrac *** realToFrac

