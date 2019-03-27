{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds, RankNTypes, NoMonomorphismRestriction #-}

module Opt where

import Numeric.AD
import qualified Data.Foldable as F
import Control.Arrow (second,(***))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Seq

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

dist :: Floating a => (a,a) -> (a,a) -> a
dist p1 p2 = sqrt $ distsq p1 p2

norm :: Floating a => V a -> a
norm v = sqrt ((sum $ fmap (^ 2) v) + epsd)

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

approxEq :: (Floating a, Ord a) => a -> a -> Bool
approxEq x y = abs (x - y) <= epsd

r2f :: (Fractional b, Real a) => a -> b
r2f = realToFrac

i2f :: Real a => (a,a) -> LSInterval
i2f = r2f *** r2f

----- Various consts

nanSub :: (Autofloat a) => a
nanSub = 0

infinity :: Floating a => a
infinity = 1/0

-- }}}

data Params = Params
  { weight        :: Weight
  , optStatus     :: EPOptStatus
  , overallObjFn  :: AugObjFn
  , iterTrace     :: IterTrace
  , epIterLimit   :: Int
  , lsIterLimit   :: Int
  , weightLimit   :: Float
  , growthFactor  :: Float
  , armijoConst   :: Float
  , wolfeConst    :: Float
  , lsInitStep    :: Float
  , lsMinInterval :: Float
  }

type Weight     = Float
type IterTrace  = Seq (Weight,LSIter,V Float)
type Iter       = V Float
type LSIter     = Seq (LSStep,LSInterval,Float)
type LSInterval = (Float,Float)
data LSStep
  = NotArmijo
  | NotWolfe
  | LimitReached
  | IntervalConverged
  | LSSuccess
  deriving (Eq,Ord,Show)

data EPOptStatus
  = NewIter
  | LSRunning EPState
  | LSConverged EPState
  | EPConverged
  deriving (Eq,Ord,Show)

type EPState = V Float

runOpt :: Autofloat a => Params -> V a -> (IterTrace,V a)
runOpt = update 0
  where
  update n params vstate
    | n >= iLimit
    = ( itrace
      , vstate
      )
    | pWeight >= wLimit
    = ( itrace
      , vstate
      )
    | otherwise
    = case optStatus params of
        NewIter ->
          update'
            ( params { optStatus = LSRunning cur
                     , weight    = initWeight
                     , iterTrace = itrace |> (pWeight,lsi,new)
                     }
            ) vstate'
  
        LSRunning prev ->
          update'
            ( params' { optStatus = LSConverged prev
                      }
            )
            -- ( if stopCond vstate vstate'
            --   then params' { optStatus = LSConverged prev
            --                }
            --   else params'
            -- )
            vstate'
          where
          params' = params { iterTrace = itrace |> (pWeight,lsi,new) }

        LSConverged prev ->
          update'
            ( if stopCond prev cur
              then params' { optStatus = EPConverged
                           }
              else params' { optStatus = LSRunning cur
                           , weight    = growth * weight params
                           }
            ) vstate
          where
          params' = params { iterTrace = itrace |> (pWeight,lsi,new) }

        EPConverged ->
          ( itrace
          , vstate
          )
      
    where
    update'  = update $ succ n
    itrace   = iterTrace params
    status   = optStatus params
    cur      = fmap r2f vstate
    new      = fmap r2f vstate'
    stopCond = epStopCond $ weightedObjFn params
    (lsi, vstate', gradEval) = stepWithObjective params vstate
    -- hparams
    iLimit   = epIterLimit params
    wLimit   = weightLimit params
    pWeight  = weight params
    growth   = growthFactor params

stepWithObjective :: (Autofloat a) => Params -> V a -> (LSIter,V a, V a)
stepWithObjective params state =
  ( lsRes
  , steppedState
  , gradEval
  )
  where
  (lsRes,timestep) =
    second catchNaN
    $ awLineSearch params f df descentDir state
  steppedState = apV (stepT timestep) state gradEval
  f          = weightedObjFn params
  df u x     = gradF x `dotV` u
  gradF      = grad f
  gradEval   = gradF state
  descentDir = negV gradEval

catchNaN :: Autofloat a => a -> a
catchNaN x =
  if isNaN x
  then nanSub
  else x

awLineSearch :: Autofloat a => Params -> ObjFn -> ObjFnD -> V a -> V a -> (LSIter,a)
awLineSearch params f df u x0 =
  update 0 (0,infinity) (mempty,initStep)
  where
  update n i@(a,b) (hist,t)
    | n >= limit
    = ( hist |> (LimitReached,i',t')
      , t
      )
    | extent i < minInterval
    = ( hist |> (IntervalConverged,i',t')
      , t
      )

    | not $ armijo t
    = update' (a,t)
      ( hist |> (NotArmijo,i',t')
      , (a + t) / 2
      )

    | not $ weakWolfe t
    = update' (t,b)
      ( hist |> (NotWolfe,i',t')
      , if b < infinity
        then (t + b) / 2
        else 2 * t
      )

    | otherwise
    = ( hist |> (LSSuccess,i',t')
      , t
      )
    where
    update' = update $ succ n
    i' = i2f i
    t' = r2f t
  duf = df u
  armijo t =
    f (x0 +. t *. u) <= f x0 + c1 * t * duf x0
  weakWolfe t =
    duf (x0 +. t *. u) >= c2 * duf x0
  extent (a,b) = abs (b - a)
  -- hparams
  c1          = r2f $ armijoConst params
  c2          = r2f $ wolfeConst params
  limit       = lsIterLimit params
  minInterval = r2f $ lsMinInterval params
  initStep    = r2f $ lsInitStep params

epStopCond :: Autofloat a => (V a -> a) -> V a -> V a -> Bool
epStopCond f x x' =
  (norm (x -. x') <= epStop) || (abs (f x - f x') <= epStop)
  where
  epStop = 10 ** (-5)

initParams :: AugObjFn -> Params
initParams f = Params
  { weight        = initWeight
  , optStatus     = NewIter
  , iterTrace     = mempty
  , overallObjFn  = f
  , epIterLimit   = 100
  , lsIterLimit   = 100
  , weightLimit   = 1e8
  , growthFactor  = 2
  , armijoConst   = 1e-4
  , wolfeConst    = 0.2
  , lsInitStep    = 1
  , lsMinInterval = 1e-10
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

-- {{{

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

-- }}}

