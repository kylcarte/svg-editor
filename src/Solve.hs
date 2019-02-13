{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import qualified Numeric.AD.Newton as AD
import Numeric.AD.Mode.Reverse (Reverse,auto)
import Numeric.AD.Internal.Reverse (Tape,Reverse(Lift))
import Data.Reflection (Reifies)

import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Either as Either

import qualified Test.QuickCheck as QC

solveHandleMove
  :: Floating a
  => ShapeType -> String
  -> (Double,Double) -> Env Double
  -> Either EvalErr [Env a]
solveHandleMove st h loc vals = do
  cf  <- mkCostFunction st h loc
  ecs <- mkEqualityConstraints st h vals
  undefined


mkCostFunction
  :: Floating a
  => ShapeType -> String
  -> (Double,Double)
  -> Either EvalErr (Env a -> a)
mkCostFunction st h loc =
  evalCostFunction
  <$> mkCostFunctionExpr st h loc

mkCostFn
  :: Floating a
  => ShapeType -> String
  -> (Double,Double)
  -> Either EvalErr (Fn a)
mkCostFn st h loc =
  evalCostFn
  <$> mkCostFunctionExpr st h loc

mkEqualityConstraints
  :: Floating a
  => ShapeType -> String
  -> Env Double
  -> Either EvalErr [Env a -> a]
mkEqualityConstraints st h vals =
  fmap (uncurry evalEqualityConstraint)
  <$> mkEqualityConstraintExprs st h vals

mkEqualityConstraintFns
  :: Floating a
  => ShapeType -> String
  -> Env Double
  -> Either EvalErr [Fn a]
mkEqualityConstraintFns st h vals =
  fmap (uncurry evalEqualityConstraintFn)
  <$> mkEqualityConstraintExprs st h vals



mkCostFunctionExpr
  :: ShapeType -> String
  -> (Double,Double)
  -> Either EvalErr Expr
mkCostFunctionExpr st h (hx,hy) = do
  env <- mkShapeEnv st
  (hxE,hyE) <- evalVar (shapeHandles st) h
  hxE' <- eval env hxE
  hyE' <- eval env hyE
  return $ euclDist
    (hxE',hyE')
    (Val hx,Val hy)

mkEqualityConstraintExprs
  :: ShapeType -> String
  -> Env Double
  -> Either EvalErr [(Expr,Double)]
mkEqualityConstraintExprs st h vals = do
  env <- mkShapeEnv st
  let fixedExprs = handleFixList st h
  mapM
    ( \e -> do
      e' <- eval env e
      (,) e' <$> eval vals e'
    ) fixedExprs

-- Helpers {{{

mkShapeEnv :: ShapeType -> Either EvalErr (Env Expr)
mkShapeEnv st =
  Map.foldlWithKey
    ( \menv x e -> menv >>= extend x e )
    menv0
    $ shapeDefs st
  where
  extend x e env = do
    v <- eval env e
    return $ Map.insert x v env
  menv0 =
    return
    $ Map.mapWithKey (const . Var)
    $ shapeParams st

euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

-- }}}



evalCostFunction
  :: Floating a
  => Expr
  -> Env a -> a
evalCostFunction =
  unsafeEval

evalCostFn
  :: Floating a
  => Expr
  -> Fn a
evalCostFn e = Fn
  $ evalCostFunction e

evalEqualityConstraint
  :: Floating a
  => Expr -> Double
  -> Env a -> a
evalEqualityConstraint e v =
  unsafeEval
  $ e - Val v

evalEqualityConstraintFn
  :: Floating a
  => Expr -> Double
  -> Fn a
evalEqualityConstraintFn e v = Fn
  $ evalEqualityConstraint e v

unsafeEval :: Floating a => Expr -> Env a -> a
unsafeEval e =
  either
    ( error . ("unexpected error in eval: " ++) . show
    ) id
  . (`eval` e)


-- Fn {{{

-- our own CC data type, to avoid orphan instances
data Fn a = Fn
  { apFn :: forall s. Reifies s Tape => Env (Reverse s a) -> Reverse s a
  }

applyFn :: (Ord a, Floating a) => Fn a -> Env a -> a
applyFn fn env = AD.eval (apFn fn) env

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
  (**)  = fn2 (**)
  sqrt  = fn1 sqrt
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

-- }}}

-- Tests {{{

testCostFunctionExpr
  :: ShapeType
  -> String
  -> ((Double,Double) -> Expr)
  -> QC.Property
testCostFunctionExpr st h e =
  QC.forAll genLoc $ \loc ->
  mkCostFunctionExpr st h loc
  QC.=== Right (e loc)

testCostFunction
  :: ShapeType -> String
  -> ((Double,Double) -> Env Double -> Double)
  -> QC.Property
testCostFunction st h cf =
  QC.forAll genLoc $ \loc ->
  QC.forAll (genEnv st) $ \env ->
    (($ env) <$> mkCostFunction st h loc)
    QC.=== Right (cf loc env)

testCostFn
  :: ShapeType -> String
  -> ((Double,Double) -> Fn Double)
  -> QC.Property
testCostFn st h cf =
  QC.forAll genLoc $ \loc ->
  QC.forAll (genEnv st) $ \env ->
    (($ env) . applyFn <$> mkCostFn st h loc)
    QC.=== Right (applyFn (cf loc) env)

testEqualityConstraintExprs
  :: ShapeType -> String
  -> (Env Double -> [(Expr,Double)])
  -> QC.Property
testEqualityConstraintExprs st h eces =
  QC.forAll (genEnv st) $ \vals ->
    mkEqualityConstraintExprs st h vals
    QC.=== Right (eces vals)

testEqualityConstraints
  :: ShapeType -> String
  -> (Env Double -> [Env Double -> Double])
  -> QC.Property
testEqualityConstraints st h ecs =
  QC.forAll (genEnv st) $ \vals ->
  QC.forAll (genEnv st) $ \env ->
    (fmap ($ env) <$> mkEqualityConstraints st h vals)
    QC.=== Right (($ env) <$> ecs vals)

-- Rectangle Tests {{{

props_rect :: [QC.Property]
props_rect =
  [ prop_cfe_rect_lt
  , prop_cfe_rect_rt
  , prop_cfe_rect_lb
  , prop_cfe_rect_rb
  , prop_cf_rect_lt
  , prop_cf_rect_rt
  , prop_cf_rect_lb
  , prop_cf_rect_rb
  , prop_cfn_rect_lt
  , prop_eces_rect_lt
  , prop_eces_rect_rt
  , prop_eces_rect_lb
  , prop_eces_rect_rb
  , prop_ecs_rect_lt
  , prop_ecs_rect_rt
  , prop_ecs_rect_lb
  , prop_ecs_rect_rb
  ]

prop_cfe_rect_lt = testCostFunctionExpr rectangle "lt" cfe_rect_lt
prop_cfe_rect_rt = testCostFunctionExpr rectangle "rt" cfe_rect_rt
prop_cfe_rect_lb = testCostFunctionExpr rectangle "lb" cfe_rect_lb
prop_cfe_rect_rb = testCostFunctionExpr rectangle "rb" cfe_rect_rb

prop_cf_rect_lt = testCostFunction rectangle "lt" cf_rect_lt
prop_cf_rect_rt = testCostFunction rectangle "rt" cf_rect_rt
prop_cf_rect_lb = testCostFunction rectangle "lb" cf_rect_lb
prop_cf_rect_rb = testCostFunction rectangle "rb" cf_rect_rb

prop_cfn_rect_lt = testCostFn rectangle "lt" cfn_rect_lt

prop_eces_rect_lt = testEqualityConstraintExprs rectangle "lt" eces_rect_lt
prop_eces_rect_rt = testEqualityConstraintExprs rectangle "rt" eces_rect_rt
prop_eces_rect_lb = testEqualityConstraintExprs rectangle "lb" eces_rect_lb
prop_eces_rect_rb = testEqualityConstraintExprs rectangle "rb" eces_rect_rb

prop_ecs_rect_lt = testEqualityConstraints rectangle "lt" ecs_rect_lt
prop_ecs_rect_rt = testEqualityConstraints rectangle "rt" ecs_rect_rt
prop_ecs_rect_lb = testEqualityConstraints rectangle "lb" ecs_rect_lb
prop_ecs_rect_rb = testEqualityConstraints rectangle "rb" ecs_rect_rb

-- Rectangle LT {{{

cfe_rect_lt :: (Double,Double) -> Expr
cfe_rect_lt (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" - "w" / 2
  hy = "cy" - "h" / 2

cf_rect_lt :: (Double,Double) -> Env Double -> Double
cf_rect_lt (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = env ! "cx"
     - env ! "w" / 2
  hy = env ! "cy"
     - env ! "h" / 2

cfn_rect_lt :: Floating a => (Double,Double) -> Fn a
cfn_rect_lt (x',y') =
  sqrt
  $ (dbl x' - hx) ** 2
  + (dbl y' - hy) ** 2
  where
  hx = var "cx"
     - var "w" / 2
  hy = var "cy"
     - var "h" / 2

nan_loc :: Floating a => (a,a)
nan_loc = (1.5597682775177049,2.0488920965818513e-2)

test_env :: Env Expr
test_env = Map.fromList
  [ (x,Var x)
  | x <- ["cx","cy","h","w"]
  ]

nan_env :: Floating a => Env a
nan_env = Map.fromList
  [ ("cx",1.7340736308565423)
  , ("cy",1.4897873302098141)
  , ("h",1.8547657855803013)
  , ("w",2.626316680040554)
  ]

eces_rect_lt :: Env Double -> [(Expr,Double)]
eces_rect_lt vals =
  [ ( "cx" + "w" / 2
    , ( (vals ! "cx")
      + (vals ! "w") / 2
      )
    )
  , ( "cy" + "h" / 2
    , ( (vals ! "cy")
      + (vals ! "h") / 2
      )
    )
  ]

ecs_rect_lt :: Env Double -> [Env Double -> Double]
ecs_rect_lt vals =
  [ \env -> (env ! "cx")
          + (env ! "w") / 2
          - ( (vals ! "cx")
            + (vals ! "w") / 2
            )
  , \env -> (env ! "cy")
          + (env ! "h") / 2
          - ( (vals ! "cy")
            + (vals ! "h") / 2
            )
  ]

-- }}}

-- Rectangle RT {{{

cfe_rect_rt :: (Double,Double) -> Expr
cfe_rect_rt (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" + "w" / 2
  hy = "cy" - "h" / 2

cf_rect_rt :: (Double,Double) -> Env Double -> Double
cf_rect_rt (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     + (env ! "w") / 2
  hy = (env ! "cy")
     - (env ! "h") / 2

eces_rect_rt :: Env Double -> [(Expr,Double)]
eces_rect_rt vals =
  [ ( "cx" - "w" / 2
    , ( (vals ! "cx")
      - (vals ! "w") / 2
      )
    )
  , ( "cy" + "h" / 2
    , ( (vals ! "cy")
      + (vals ! "h") / 2
      )
    )
  ]

ecs_rect_rt :: Env Double -> [Env Double -> Double]
ecs_rect_rt vals =
  [ \env -> (env ! "cx")
          - (env ! "w") / 2
          - ( (vals ! "cx")
            - (vals ! "w") / 2
            )
  , \env -> (env ! "cy")
          + (env ! "h") / 2
          - ( (vals ! "cy")
            + (vals ! "h") / 2
            )
  ]

-- }}}

-- Rectangle LB {{{

cfe_rect_lb :: (Double,Double) -> Expr
cfe_rect_lb (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" - "w" / 2
  hy = "cy" + "h" / 2

cf_rect_lb :: (Double,Double) -> Env Double -> Double
cf_rect_lb (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     - (env ! "w") / 2
  hy = (env ! "cy")
     + (env ! "h") / 2

eces_rect_lb :: Env Double -> [(Expr,Double)]
eces_rect_lb vals =
  [ ( "cx" + "w" / 2
    , ( (vals ! "cx")
      + (vals ! "w") / 2
      )
    )
  , ( "cy" - "h" / 2
    , ( (vals ! "cy")
      - (vals ! "h") / 2
      )
    )
  ]

ecs_rect_lb :: Env Double -> [Env Double -> Double]
ecs_rect_lb vals =
  [ \env -> (env ! "cx")
          + (env ! "w") / 2
          - ( (vals ! "cx")
            + (vals ! "w") / 2
            )
  , \env -> (env ! "cy")
          - (env ! "h") / 2
          - ( (vals ! "cy")
            - (vals ! "h") / 2
            )
  ]

-- }}}

-- Rectangle RB {{{

cfe_rect_rb :: (Double,Double) -> Expr
cfe_rect_rb (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" + "w" / 2
  hy = "cy" + "h" / 2

cf_rect_rb :: (Double,Double) -> Env Double -> Double
cf_rect_rb (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     + (env ! "w") / 2
  hy = (env ! "cy")
     + (env ! "h") / 2

eces_rect_rb :: Env Double -> [(Expr,Double)]
eces_rect_rb vals =
  [ ( "cx" - "w" / 2
    , ( (vals ! "cx")
      - (vals ! "w") / 2
      )
    )
  , ( "cy" - "h" / 2
    , ( (vals ! "cy")
      - (vals ! "h") / 2
      )
    )
  ]

ecs_rect_rb :: Env Double -> [Env Double -> Double]
ecs_rect_rb vals =
  [ \env -> (env ! "cx")
          - (env ! "w") / 2
          - ( (vals ! "cx")
            - (vals ! "w") / 2
            )
  , \env -> (env ! "cy")
          - (env ! "h") / 2
          - ( (vals ! "cy")
            - (vals ! "h") / 2
            )
  ]

-- }}}

-- }}}

-- RotatableRectangle Tests {{{

props_rot_rect :: [QC.Property]
props_rot_rect =
  [ prop_cfe_rot_rect_lt
  , prop_cf_rot_rect_lt
  , prop_eces_rot_rect_lt
  , prop_ecs_rot_rect_lt
  ]

prop_cfe_rot_rect_lt = testCostFunctionExpr rotatableRectangle "lt" cfe_rot_rect_lt

prop_cf_rot_rect_lt = testCostFunction rotatableRectangle "lt" cf_rot_rect_lt

prop_eces_rot_rect_lt = testEqualityConstraintExprs rotatableRectangle "lt" eces_rot_rect_lt

prop_ecs_rot_rect_lt = testEqualityConstraints rotatableRectangle "lt" ecs_rot_rect_lt

-- RotatableRectangle LT {{{

cfe_rot_rect_lt :: (Double,Double) -> Expr
cfe_rot_rect_lt (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" + (- "w" / 2) * cos "theta" - (- "h" / 2) * sin "theta"
  hy = "cy" + (- "w" / 2) * sin "theta" + (- "h" / 2) * cos "theta"

cf_rot_rect_lt :: (Double,Double) -> Env Double -> Double
cf_rot_rect_lt (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     - (env ! "w") / 2 * cos (env ! "theta")
     + (env ! "h") / 2 * sin (env ! "theta")
  hy = (env ! "cy")
     - (env ! "w") / 2 * sin (env ! "theta")
     - (env ! "h") / 2 * cos (env ! "theta")

eces_rot_rect_lt :: Env Double -> [(Expr,Double)]
eces_rot_rect_lt vals =
  [ ( "cx" + "w" / 2 * cos "theta"
           - "h" / 2 * sin "theta"
    , (vals ! "cx")
      + (vals ! "w") / 2 * cos (vals ! "theta")
      - (vals ! "h") / 2 * sin (vals ! "theta")
    )
  , ( "cy" + "w" / 2 * sin "theta"
           + "h" / 2 * cos "theta"
    , (vals ! "cy")
      + (vals ! "w") / 2 * sin (vals ! "theta")
      + (vals ! "h") / 2 * cos (vals ! "theta")
    )
  , ( "theta"
    , vals ! "theta"
    )
  ]

ecs_rot_rect_lt :: Env Double -> [Env Double -> Double]
ecs_rot_rect_lt vals =
  [ \env ->
      env ! "cx"
      + env ! "w" / 2 * cos (env ! "theta")
      - env ! "h" / 2 * sin (env ! "theta")
      - ( (vals ! "cx")
        + (vals ! "w") / 2 * cos (vals ! "theta")
        - (vals ! "h") / 2 * sin (vals ! "theta")
        )
  , \env ->
      env ! "cy"
      + env ! "w" / 2 * sin (env ! "theta")
      + env ! "h" / 2 * cos (env ! "theta")
      - ( (vals ! "cy")
        + (vals ! "w") / 2 * sin (vals ! "theta")
        + (vals ! "h") / 2 * cos (vals ! "theta")
        )
     
  , \env ->
      env ! "theta"
      - vals ! "theta"
  ]

-- }}}

-- RotatableRectangle RT {{{

cfe_rot_rect_rt :: (Double,Double) -> Expr
cfe_rot_rect_rt (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" + "w" / 2 * cos "theta" + "h" / 2 * sin "theta"
  hy = "cy" + "w" / 2 * sin "theta" - "h" / 2 * cos "theta"

cf_rot_rect_rt :: (Double,Double) -> Env Double -> Double
cf_rot_rect_rt (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     + (env ! "w") / 2 * cos (env ! "theta")
     + (env ! "h") / 2 * sin (env ! "theta")
  hy = (env ! "cy")
     + (env ! "w") / 2 * sin (env ! "theta")
     - (env ! "h") / 2 * cos (env ! "theta")

-- }}}

-- RotatableRectangle LB {{{

cfe_rot_rect_lb :: (Double,Double) -> Expr
cfe_rot_rect_lb (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" - "w" / 2 * cos "theta" - "h" / 2 * sin "theta"
  hy = "cy" - "w" / 2 * sin "theta" + "h" / 2 * cos "theta"

cf_rot_rect_lb :: (Double,Double) -> Env Double -> Double
cf_rot_rect_lb (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     - (env ! "w") / 2 * cos (env ! "theta")
     - (env ! "h") / 2 * sin (env ! "theta")
  hy = (env ! "cy")
     - (env ! "w") / 2 * sin (env ! "theta")
     + (env ! "h") / 2 * cos (env ! "theta")

-- }}}

-- RotatableRectangle RB {{{

cfe_rot_rect_rb :: (Double,Double) -> Expr
cfe_rot_rect_rb (x',y') =
  sqrt
  $ (Val x' - hx) ** 2
  + (Val y' - hy) ** 2
  where
  hx = "cx" + "w" / 2 * cos "theta" - "h" / 2 * sin "theta"
  hy = "cy" + "w" / 2 * sin "theta" + "h" / 2 * cos "theta"

cf_rot_rect_rb :: (Double,Double) -> Env Double -> Double
cf_rot_rect_rb (x',y') env =
  sqrt
  $ (x' - hx) ** 2
  + (y' - hy) ** 2
  where
  hx = (env ! "cx")
     + (env ! "w") / 2 * cos (env ! "theta")
     - (env ! "h") / 2 * sin (env ! "theta")
  hy = (env ! "cy")
     + (env ! "w") / 2 * sin (env ! "theta")
     + (env ! "h") / 2 * cos (env ! "theta")

-- }}}

-- }}}

genLoc :: QC.Gen (Double,Double)
genLoc = (,) <$> g <*> g
  where
  g = QC.scale (2 *) $ QC.arbitrary `QC.suchThat` (>= 0)

genEnv :: ShapeType -> QC.Gen (Env Double)
genEnv = sequenceA . (g <$) . shapeParams
  where
  g = QC.scale (2 *) $ QC.arbitrary `QC.suchThat` (>= 0)

-- }}}

{-
-- {{{

data ConOptProblem a = ConOptProblem
  { costFunction    :: Fn a
  , eqConstraints   :: [(Fn a, a)]
  }

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

{-
test_MC :: Either EvalErr ((Double,Double) -> [Env Double])
test_MC = allTogetherNow
  rotatableRectangle
  "r"
  $ ShapeVal "rotatableRectangle"
  $ Map.fromList
  [ ("w" , 50)
  , ("h" , 50)
  , ("cx" , 40)
  , ("cy" , 40)
  , ("theta" , 0)
  ]
-}

{-
test_MC2 :: Either EvalErr ((Double,Double) -> ConOptProblem Double)
test_MC2 = moveControl
  rotatableRectangle
  "r"
  $ ShapeVal "rotatableRectangle"
  $ Map.fromList
  [ ("w" , 50)
  , ("h" , 50)
  , ("cx" , 40)
  , ("cy" , 40)
  , ("theta" , 0)
  ]
-}

-- TODO: write test on test_MC: returns a Right, doesn't give NaNs.

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

{-
( "cx"
+ ((Neg ("w" * 0.5)) * Cos "theta")
- ((Neg ("h" * 0.5)) * Sin "theta")
)

(Add
  (Add "cx"
    ((Neg ("w" * 0.5)) * Cos "theta"))
  (Neg ((Neg ("h" * 0.5)) * Sin "theta")))
(Add
  (Add "cy"
    ((Neg ("w" * 0.5)) * Sin "theta"))
  ((Neg ("h" * 0.5)) * Cos "theta"))
(Add
  (Add "cy"
    ((Neg ("w" * 0.5)) * Sin "theta"))
  ((Neg ("h" * 0.5)) * Cos "theta"))

( "cx"
  + ((Neg ("w" * 0.5)) * Cos "theta")
  - ((Neg ("h" * 0.5)) * Sin "theta")
)

(Add
  (Add "cx"
    (Neg (("w" * 0.5) * Cos "theta")))
  ("h" * 0.5) * Sin "theta"))
(Add
  (Add "cy"
    (Neg (("w" * 0.5) * Sin "theta")))
  (Neg (("h" * 0.5) * Cos "theta")))
(Add
  (Add "cy"
    (Neg (("w" * 0.5) * Sin "theta")))
  (Neg (("h" * 0.5) * Cos "theta")))
-}


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

checkDefs :: Floating a => ShapeType -> Either EvalErr (Env (Fn a))
checkDefs st = traverse (check $ shapeParams st) $ shapeDefs st

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



-- }}}
-}

