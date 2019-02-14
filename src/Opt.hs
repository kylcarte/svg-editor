{-# LANGUAGE ConstraintKinds, RankNTypes, NoMonomorphismRestriction #-}

module Opt where

import Numeric.AD
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map

-- Utils {{{

------ Opt types, util functions, and params
type V = Map String
type Autofloat a = (RealFloat a, Ord a)
type AugObjFn = forall a. Autofloat a => a -> V a -> a
type ObjFn    = forall a. Autofloat a => V a -> a
type ObjFnD   = forall a. Autofloat a => V a -> V a -> a
type GradFn   = forall a. Autofloat a => V a -> V a

infixl 6 +., -.
infixl 7 *.

-- assumes lists are of the same length
dotV :: Num a => V a -> V a -> a
dotV u v = sum $ Map.intersectionWith (*) u v

(+.) :: Num a => V a -> V a -> V a -- add two vectors
(+.) = Map.unionWith (+)

negV :: Num a => V a -> V a
negV = fmap negate

(-.) :: Num a => V a -> V a -> V a -- subtract two vectors
(-.) u = (u +.) . negV

(*.) :: Num a => a -> V a -> V a -- multiply by a constant
(*.) c v = fmap ((*) c) v

apV :: (a -> b -> c) -> V a -> V b -> V c
apV = Map.intersectionWith

distsq :: Num a => (a, a) -> (a, a) -> a -- distance
distsq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

norm :: Floating a => V a -> a
norm v = sqrt ((sum $ fmap (^ 2) v) + epsd)

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

r2f :: (Fractional b, Real a) => a -> b
r2f = realToFrac

----- Various consts

nanSub :: (Autofloat a) => a
nanSub = 0

infinity :: Floating a => a
infinity = 1/0

-- }}}

data Params = Params
  { weight       :: Float
  , optStatus    :: OptStatus
  , overallObjFn :: AugObjFn
  }

data OptStatus
  = NewIter
  | UnconstrainedRunning LastEPState
  | UnconstrainedConverged LastEPState
  | EPConverged
  deriving (Eq,Ord,Show)

type LastEPState = V Float

optimize :: Autofloat a => Params -> V a -> V a
optimize params vstate =
  fst $ head
  $ dropWhile
    ( not
    . (== EPConverged)
    . optStatus
    . snd
    )
  $ iterate step
  (vstate,params)

step :: (Autofloat a) => (V a,Params) -> (V a, Params)
step (vstate,params) =
  case optStatus params of
    NewIter ->
      ( vstate'
      , params
        { weight    = initWeight
        , optStatus = UnconstrainedRunning vstateMono
        }
      )

    UnconstrainedRunning lastEPstate
      | stopCond vstate vstate' ->
        ( vstate'
        , params
          { optStatus = UnconstrainedConverged lastEPstate
          }
        )
      | otherwise ->
        ( vstate'
        , params
        )

    UnconstrainedConverged lastEPstate
      | stopCond lastEPstate vstateMono ->
        ( vstate
        , params
          { optStatus = EPConverged
          }
        )
      | otherwise ->
        ( vstate
        , params
          { weight = weightGrowthFactor * weight params
          , optStatus = UnconstrainedRunning vstateMono
          }
        )

    EPConverged ->
      ( vstate
      , params
      )
  where
  (vstate', gradEval) = stepWithObjective params vstate
  stopCond = epStopCond $ weightedObjFn params
  vstateMono = fmap r2f vstate
  weightGrowthFactor = 10

stepWithObjective :: (Autofloat a) => Params -> V a -> (V a, V a)
stepWithObjective params state =
  ( steppedState
  , gradEval
  )
  where
  timestep =
    let resT = awLineSearch f df descentDir state
    in
    if isNaN resT
    then nanSub
    else resT
  steppedState = apV (stepT timestep) state gradEval
  f          = weightedObjFn params
  df u x     = gradF x `dotV` u
  gradF      = grad f
  gradEval   = gradF state
  descentDir = negV gradEval

awLineSearch :: Autofloat a => ObjFn -> ObjFnD -> V a -> V a -> a
awLineSearch f df u x0 =
  update 0 (0,infinity) 1
  where
  update n i@(a,b) t
    | n > maxIteration || extent i < minInterval
    = t

    | not $ armijo t
    = update' (a,t)
      $ (a + t) / 2

    | not $ weakWolfe t
    = update' (t,b)
      $ if b < infinity
        then (t + b) / 2
        else 2 * t

    | otherwise
    = t
    where
    update' = update (n + 1)
  duf = df u
  armijo t =
    f (x0 +. t *. u) <= f x0 + c1 * t * duf x0
  weakWolfe t =
    duf (x0 +. t *. u) >= c2 * duf x0
  extent (a,b) = abs (b - a)
  -- hyperparameters
  c1 = 0.4
  c2 = 0.2
  minInterval = 10 ** (-10)
  maxIteration = 100

epStopCond :: Autofloat a => (V a -> a) -> V a -> V a -> Bool
epStopCond f x x' =
  (norm (x -. x') <= epStop) || (abs (f x - f x') <= epStop)
  where
  epStop = 10 ** (-5)

initParams :: AugObjFn -> Params
initParams f = Params
  { weight       = initWeight
  , optStatus    = NewIter
  , overallObjFn = f
  }

initWeight :: Floating a => a
initWeight = 10 ** (-3)

penalty :: Autofloat a => a -> a
penalty x = max 0 x ^ 2

penalties :: Autofloat a => a -> [a] -> a
penalties c = (c *) . sum . map penalty

stepT :: Num a => a -> a -> a -> a
stepT dt x dfdx = x - dt * dfdx

weightedObjFn :: Params -> ObjFn
weightedObjFn params = overallObjFn params $ r2f $ weight params

{-
test0 :: V Float
test0 = optimize
  ( initParams $ \c [w,h,cx,cy] ->
      distsq (40,40) (cx,cy)
      + penalties c
        [ 75 - (cy + h/2)
        , 5  - (cx - w/2)
        , (cy + h/2) - 75
        , (cx - w/2) -  5
        ]
  )
  [ 50 -- w
  , 30 -- h
  , 30 -- cx
  , 60 -- cy
  ]

test_rot_rect_converging :: V Float
test_rot_rect_converging = optimize
  ( initParams $ \c [w,h,cx,cy,theta] ->
    distsq (50,50) (cx + 10 * cos theta,cy + 10 * sin theta)
    + penalties c
      [ 50 - w  , w  - 50
      , 30 - h  , h  - 30
      , 40 - cx , cx - 40
      , 40 - cy , cy - 40
      ]
  )
  [ 50 -- w
  , 30 -- h
  , 40 -- cx
  , 40 -- cy
  , 0  -- theta
  ]

test_rot_rect_diverging :: V Float
test_rot_rect_diverging = optimize
  ( initParams $ \c [w,h,cx,cy,theta] ->
    distsq (50,60) (cx + 10 * cos theta,cy + 10 * sin theta)
    + penalties c
      [ 50 - w  , w  - 50
      , 30 - h  , h  - 30
      , 40 - cx , cx - 40
      , 40 - cy , cy - 40
      ]
  )
  [ 50 -- w
  , 30 -- h
  , 40 -- cx
  , 40 -- cy
  , 0  -- theta
  ]
-}

