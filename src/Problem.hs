{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Problem where

import Spec
import Expr
import Opt

import Control.Arrow ((***))
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.AD

-- TODO: control where typechecking happens, make evaluating safe

moveHandle :: ShapeType -> Env Float -> Handle -> (Float,Float) -> (IterTrace,Env Float) -- (Env Float,String)
moveHandle st env h p =
  runOpt (genProblem st env h p) env

genProblem :: ShapeType -> Env Float -> Handle -> (Float,Float) -> Params
genProblem st env h p =
  initParams objFn
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

inspectObjFn :: Env Ty -> Params -> Expr
inspectObjFn vars params = objFn cPen absVars
  where
  objFnD = grad $ \env -> objFn (env Map.! "cPenalty") env
  objFn :: AugObjFn
  objFn   = overallObjFn params
  absVars = Map.mapWithKey (const . Var) vars
  cPen    = Var "cPenalty"

inspectObjFnD :: Env Ty -> Float -> Params -> Env Expr
inspectObjFnD vars c params = objFnD absVars
  where
  objFnD = grad $ \env -> overallObjFn params (r2f c) env
  absVars = Map.mapWithKey (const . Var) vars

evalFixList :: (Floating a, Ord a) => ShapeType -> Env a -> Handle -> Either EvalErr [(Expr,a)]
evalFixList st env h =
  traverse
  ( \e -> (,) e <$> evalWithShape st env e
  ) $ handleFixList st h

evalHandle :: (Floating a, Ord a) => ShapeType -> Env a -> Handle -> Either EvalErr (a,a)
evalHandle st env h = do
  (ex,ey) <- maybe (Left $ UnboundVar h) Right
             $ Map.lookup h
             $ shapeHandles st
  (,) <$> ev ex <*> ev ey
  where
  ev = evalWithShape st env

abstractPt :: Fractional a => (Float,Float) -> (a,a)
abstractPt = realToFrac *** realToFrac

testObjFn_rot_rect_r_1 :: Expr
testObjFn_rot_rect_r_1 =
  inspectObjFn (shapeParams st)
  $ genProblem st env h p
  where
  st  = rotatableRectangle
  env = Map.fromList
          [ ( "w"     , 90 )
          , ( "h"     , 30 )
          , ( "cx"    , 0 )
          , ( "cy"    , 0 )
          , ( "theta" , (-pi) / 5 )
          ]
  h   = "r"
  p   = (74/3,(-17.0))

{-

(12932437 / 524288 - cx - 30 * cos theta) ^ 2
+ (-17 + (- (cy + 30 * sin theta))) * (-17 + (- (cy + 30 * sin theta)))
+ cPenalty *
  ( 0
  + (max 0 (w - 90)) ^ 2
  + (max 0 (h - 30)) ^ 2
  + (max 0 (cx - 0)) ^ 2
  + (max 0 (cy - 0)) ^ 2
  + (max 0 (90 - w)) ^ 2
  + (max 0 (30 - h)) ^ 2
  + (max 0 (0 - cx)) ^ 2
  + (max 0 (0 - cy)) ^ 2
  )

-}

testObjFnD_rot_rect_r_1 :: Env Expr
testObjFnD_rot_rect_r_1 =
  inspectObjFnD (shapeParams st) 10000
  $ genProblem st env h p
  where
  st  = rotatableRectangle
  env = Map.fromList
          [ ( "w"     , 90 )
          , ( "h"     , 30 )
          , ( "cx"    , 0 )
          , ( "cy"    , 0 )
          , ( "theta" , (-pi) / 5 )
          ]
  h   = "r"
  p   = (74/3,(-17.0))

{-

[ ( "cx"
  , "( (-1) * (((- cx)) * 10000 + ((- cx)) * 10000)
     + ((cx + 0) * 10000 + (cx + 0) * 10000)
     + ((-1) * ( (12932437 / 524288 + (- (cx + 30 * cos theta)))
               + (12932437 / 524288 + (- (cx + 30 * cos theta)))
               )
       )
     )"
  ==> 44 + 2/3 - 2 * cx - 60 * cos theta
  )
, ( "cy"
  , "( (-1) * (2 * ((- cy)) * 10000)
     + (2 * (cy + 0) * 10000)
     + ((34 + 2 * cy + 60 * sin theta
        )
       )
     )"
  ==> 34 + 2 * cy + 60 * sin theta
  )
, ( "h"
  , "( (-1) * ((30 + (- h)) * 10000 + (30 + (- h)) * 10000)
     + 1 * ((h + (-30)) * 10000 + (h + (-30)) * 10000)
     )"
  ==> 0
  )
, ( "theta"
  , "( 1020 * cos theta
     + (193986555 / 131072) * sin theta
     - 30 * cx * sin theta
     + 60 * cy * cos theta
     + 900 * sin theta * cos theta
     )"
  )
, ( "w"
  , "( (-1) * ( 2 * (90 - w) * 10000
              )
            + ( 2 * (w - 90) * 10000
              )
     )"
  ==> 0
  )
]

-}

testFixList_rot_rect_r_1 :: Either EvalErr [(Expr,Float)]
testFixList_rot_rect_r_1 =
  evalFixList st env h
  where
  st  = rotatableRectangle
  env = Map.fromList
          [ ( "w"     , 90 )
          , ( "h"     , 30 )
          , ( "cx"    , 0 )
          , ( "cy"    , 0 )
          , ( "theta" , (-pi) / 5 )
          ]
  h   = "r"
  p   = (74/3,(-17.0))


{-

(74 / 3 - (cx + 30 * cos theta)) * (74 / 3 - (cx + 30 * cos theta))
+ (-17 - (cy + 30 * sin theta)) * (-17 - (cy + 30 * sin theta))
+ cPenalty *
  ( 0 
  + (max 0 (cx - 0)) ^ 2
  + (max 0 (cy - 0)) ^ 2
  + (max 0 (h - 30)) ^ 2
  + (max 0 (w - 90)) ^ 2
  + (max 0 (0 - cx)) ^ 2
  + (max 0 (0 - cy)) ^ 2
  + (max 0 (30 - h)) ^ 2
  + (max 0 (90 - w)) ^ 2
  )

-}


