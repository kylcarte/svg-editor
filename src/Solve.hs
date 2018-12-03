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
import Numeric.AD.Mode.Reverse (Reverse,auto)
import Numeric.AD.Internal.Reverse (Tape)
import Data.Reflection (Reifies)

import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as F

mkCostFnExpr :: ShapeType -> String -> Either EvalErr (String,String,Expr)
mkCostFnExpr st h = do
  env <- mkShapeEnv st
  (hxE,hyE) <- evalVar (shapeHandles st) h
  let hx = h ++ "_x"
      hy = h ++ "_y"
  hxE' <- eval env hxE
  hyE' <- eval env hyE
  return
    ( hx
    , hy
    , euclDist (hxE',hyE') (Var hx,Var hy)
    )

testCostFn :: IO ()
testCostFn = runTests (uncurry mkCostFnExpr)
  [ ( ( rectangle , "lt" )
    , Right
      ( "lt_x"
      , "lt_y"
      , sqrt
        $ ("lt_x" - ("cx" - "w" / 2)) ** 2
        + ("lt_y" - ("cy" - "h" / 2)) ** 2
      )
    )
  , ( ( rotatableRectangle , "r" )
    , Right
      ( "r_x"
      , "r_y"
      , sqrt
        $ ("r_x" - ("cx" + 10 * cos "theta")) ** 2
        + ("r_y" - ("cy" + 10 * sin "theta")) ** 2
      )
    )
  ]

mkShapeEnv :: ShapeType -> Either EvalErr (Env Expr)
mkShapeEnv st =
  extendEnv (shapeDefs st)
  $ Map.mapWithKey (const . Var)
  $ shapeParams st

mkEqualityConstraintExprs :: ShapeType -> String -> Env Double -> Either EvalErr [(Expr,Double)]
mkEqualityConstraintExprs st h vals = do
  env <- mkShapeEnv st
  let fixedExprs = handleFixList st h
  mapM
    ( \e -> do
      e' <- eval env e
      (,) e' <$> eval vals e'
    ) fixedExprs

testEqualityConstraints :: IO ()
testEqualityConstraints = runTests (\(st,h,vals) -> mkEqualityConstraintExprs st h vals)
  [ ( ( rotatableRectangle , "r"
      , Map.fromList [("w", 40),("h", 40),("cx", 50),("cy", 30),("theta",0)] )
    , Right
      [ ( "cx" , 50 )
      , ( "cy" , 30 )
      , ( "h" , 40 )
      , ( "w" , 40 )
      ]
    )
  , ( ( rectangle , "lb"
      , Map.fromList [("w", 75),("h", 25),("cx", 30),("cy", 30)]
      )
    , Right
      [ ( ("cx" + ("w" / 2)) , 67.5 )
      , ( ("cy" - ("h" / 2)) , 17.5 )
      ]
    )
  , ( ( rotatableRectangle , "lt"
      , Map.fromList [("w", 40),("h", 40),("cx", 50),("cy", 30),("theta",0)]
      )
    , Right
      [ ( ("cx" + ((- "w" / 2) * cos "theta")) - (("h" / 2) * sin "theta") , 30 )
      , ( ("cy" + ((- "w" / 2) * sin "theta")) + (("h" / 2) * cos "theta") , 50 )
      , ( "theta" , 0 )
      ]
    )
  ]

extendEnv :: Floating a => Env Expr -> Env a -> Either EvalErr (Env a)
extendEnv es env =
  Map.foldlWithKey extM (return env) es
  where
  extM envM x e = envM >>= \env -> do
    v <- eval env e
    return $ Map.insert x v env

euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

runTests :: (Eq outp,Show outp) => (inp -> outp) -> [(inp,outp)] -> IO ()
runTests f = mapM_ (uncurry runTest) . zip [1..]
  where
  runTest i (input,expected) = do
    putStr $ "test " ++ show i ++ " ... "
    let actual = f input
    if expected == actual
      then putStrLn "passed"
      else do
        putStrLn "failed:"
        putStrLn $ "  expected: " ++ show expected
        putStrLn $ "  actual: " ++ show actual

{-
cd_test0 :: [(Double,Env Double)]
cd_test0 = constrainedDescent
  ( \env -> euclDist
    ( 10 * cos (env ! "theta") + (env ! "cx")
    , 10 * sin (env ! "theta") + (env ! "cy")
    )
    ( 50
    , 50
    )
  )
  [ CC $ \env -> (env ! "cx") - 40
  , CC $ \env -> (env ! "cy") - 40
  , CC $ \env -> (env ! "w") - 20
  , CC $ \env -> (env ! "h") - 20
  ] $ Map.fromList
  [ ("cx",40)
  , ("cy",40)
  , ("w",20)
  , ("h",20)
  , ("theta",0)
  ]
-}

{-
gd_test0 :: [(Double,Env Double)]
gd_test0 = constrainedDescent
  ( \env ->
    euclDist
    ( 10 * cos (env ! "theta") + (env ! "cx")
    , 10 * sin (env ! "theta") + (env ! "cy")
    )
    ( 50
    , 50
    )
    - (env ! "l_1") * ((env ! "cx") - 40)
    - (env ! "l_2") * ((env ! "cy") - 40)
    - (env ! "l_3") * ((env ! "w") - 20)
    - (env ! "l_4") * ((env ! "h") - 20)
  )
  [ CC $ \env ->
      (env ! "l_1") * ((env ! "cx") - 40)
    + (env ! "l_2") * ((env ! "cy") - 40)
    + (env ! "l_3") * ((env ! "w") - 20)
    + (env ! "l_4") * ((env ! "h") - 20)
    - euclDist
    ( 10 * cos (env ! "theta") + (env ! "cx")
    , 10 * sin (env ! "theta") + (env ! "cy")
    )
    ( 50
    , 50
    )
  ]
  $ Map.fromList
  [ ("cx",40)
  , ("cy",40)
  , ("w",20)
  , ("h",20)
  , ("theta",0)
  , ("l_1",1)
  , ("l_2",1)
  , ("l_3",1)
  , ("l_4",1)
  ]
-}

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

