{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module Opt where

import Numeric.AD

-- Utils {{{

------ Opt types, util functions, and params
type Autofloat a = (RealFloat a, Ord a)
type ObjFn1 a = forall a . (Autofloat a) => [a] -> a
type ObjFn2 a = forall a . (Autofloat a) => [a] -> [a] -> a
type GradFn a = forall a . (Autofloat a) => [a] -> [a]

infixl 6 +., -.
infixl 7 *.

-- assumes lists are of the same length
dotL :: (RealFloat a, Floating a) => [a] -> [a] -> a
dotL u v = if not $ length u == length v
           then error $ "can't dot-prod different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else sum $ zipWith (*) u v

(+.) :: Floating a => [a] -> [a] -> [a] -- add two vectors
(+.) u v = if not $ length u == length v
           then error $ "can't add different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else zipWith (+) u v

(-.) :: Floating a => [a] -> [a] -> [a] -- subtract two vectors
(-.) u v = if not $ length u == length v
           then error $ "can't subtract different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else zipWith (-) u v

negL :: Floating a => [a] -> [a]
negL = map negate

(*.) :: Floating a => a -> [a] -> [a] -- multiply by a constant
(*.) c v = map ((*) c) v

distsq :: Floating a => (a, a) -> (a, a) -> a -- distance
distsq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

norm :: Floating a => [a] -> a
norm v = sqrt ((sum $ map (^ 2) v) + epsd)

r2f :: (Fractional b, Real a) => a -> b
r2f = realToFrac

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

----- Various consts

nanSub :: (Autofloat a) => a
nanSub = 0

infinity :: Floating a => a
infinity = 1/0

-- }}}

data Params = Params
  { weight :: Float
  , optStatus :: OptStatus
  , overallObjFn :: forall a. Autofloat a => a -> [a] -> a
  }

data OptStatus
  = NewIter
  | UnconstrainedRunning LastEPState
  | UnconstrainedConverged LastEPState
  | EPConverged
  deriving (Eq,Ord,Show)

type LastEPState = [Float]

optimize :: Autofloat a => Params -> [a] -> [a]
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

step :: (Autofloat a) => ([a],Params) -> ([a], Params)
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
  vstateMono = map r2f vstate
  weightGrowthFactor = 10

stepWithObjective :: (Autofloat a) => Params -> [a] -> ([a], [a])
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
  steppedState = zipWith (stepT timestep) state gradEval
  f          = weightedObjFn params
  df u x     = gradF x `dotL` u
  gradF      = grad f
  gradEval   = gradF state
  descentDir = negL gradEval

awLineSearch :: (Autofloat b) => ObjFn1 a -> ObjFn2 a -> [b] -> [b] -> b
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

epStopCond :: Autofloat a => ([a] -> a) -> [a] -> [a] -> Bool
epStopCond f x x' =
  (norm (x -. x') <= epStop) || (abs (f x - f x') <= epStop)
  where
  epStop = 10 ** (-5)

initParams :: (forall a. Autofloat a => a -> [a] -> a) -> Params
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

weightedObjFn :: Autofloat a => Params -> [a] -> a
weightedObjFn params = overallObjFn params $ r2f $ weight params

test0 :: [Float]
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

test_rot_rect_converging :: [Float]
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

test_rot_rect_diverging :: [Float]
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

