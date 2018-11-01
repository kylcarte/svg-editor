{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Path where

import Numeric.AD.Newton
import Data.Monoid (Sum(..))

data Cmd a
  = MoveTo Bool a a
  | LineTo Bool a a
  | Close
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

type R      = Double
type Path a = [Cmd a]

_m, _M, _l, _L :: a -> a -> Cmd a
_m = MoveTo False
_M = MoveTo True
_l = LineTo False
_L = LineTo True

_z :: Cmd a
_z = Close






{-
-- V2 {{{

data V2 a = V2 { _x, _y :: a }
  deriving (Eq,Ord,Show)

instance Functor V2 where
  fmap f a = V2 (f $ _x a) (f $ _y a)

instance Foldable V2 where
  foldMap f a = f (_x a) <> f (_y a)

instance Traversable V2 where
  traverse f a = V2 <$> f (_x a) <*> f (_y a)

instance Num a => Num (V2 a) where
  a + b         = V2 (_x a + _x b) (_y a + _y b)
  a * b         = V2 (_x a * _x b) (_y a * _y b)
  negate        = fmap negate
  abs           = fmap abs
  signum        = fmap signum
  fromInteger i = V2 x x
    where
    x = fromInteger i

instance Fractional a => Fractional (V2 a) where
  a / b = V2 (_x a / _x b) (_y a / _y b)
  fromRational r = V2 x x
    where
    x = fromRational r

-- }}}

(===) :: Num a => a -> a -> a
(===) = (-)
infix 4 ===

{-
quad :: Num a => V2 a -> a
quad a = _x a ^ 2 + _y a ^ 2
-}

newtype ListF f a = ListF
  { listF :: [f a]
  } deriving (Eq,Ord,Show)

instance Functor f => Functor (ListF f) where
  fmap f = ListF . fmap (fmap f) . listF

instance Foldable f => Foldable (ListF f) where
  foldMap f = foldMap (foldMap f) . listF

instance Traversable f => Traversable (ListF f) where
  traverse f = fmap ListF . traverse (traverse f) . listF

sumSqrs :: (Foldable t, Num a) => t a -> a
sumSqrs = getSum . foldMap (Sum . (^ 2))

test :: [[Double]]
test = gradientDescent
  ( \[w,h,cx,cy] -> sumSqrs
    [ cx - (w / 2) === 0
    , cy - (h / 2) === 0
    , cx + (w / 2) === 50
    , cy + (h / 2) === 60
    ]
  )
  [ 40
  , 30
  , 20
  , 15
  ]

truncateStable :: (Fractional a, Ord a) => [[a]] -> [[a]]
truncateStable = \case
  x : l@(y : _)
    | and (zipWithDefault False approxEq x y) -> [x]
    | otherwise    -> x : truncateStable l
  l -> l

zipWithDefault :: c -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault d f = \case
  []           -> (d <$)
  as@(a : as') -> \case
    []      -> d <$ as
    b : bs' -> f a b : zipWithDefault d f as' bs'

approxEq :: (Fractional a, Ord a) => a -> a -> Bool
approxEq a b = abs (a - b) < epsilon

epsilon :: Fractional a => a
epsilon = 1e-12

{-
test :: [ListF V2 Double]
test = gradientDescent
  ( \(ListF [d,c,tl,tr,br,bl]) ->
    sum $ quad <$>
    [ tl === c + V2 (- _x d / 2) (- _y d / 2)
    , tr === c + V2   (_x d / 2) (- _y d / 2)
    , br === c + V2   (_x d / 2)   (_y d / 2)
    , bl === c + V2 (- _x d / 2)   (_y d / 2)
    , tl === V2 0 0
    , br === V2 50 50
    ]
  ) $ ListF
  [ V2 40 30
  , V2 20 15
  , V2 0 0
  , V2 40 0
  , V2 40 30
  , V2 0 30
  ]
  -- ( \[ox,oy,w,h,ax,ay,bx,by,cx,cy,dx,dy] ->
  --   sum $ (^ 2) <$>
  --   [ ax === ox - (w / 2)
  --   , ay === oy - (h / 2)
  --   , bx === ox + (w / 2)
  --   , by === oy - (h / 2)
  --   , cx === ox + (w / 2)
  --   , cy === oy + (h / 2)
  --   , dx === ox - (w / 2)
  --   , dy === oy + (h / 2)
  --   ]
  -- )
  -- [20,15,40,30,]
-}
-}










{-
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.String (IsString(..))

type Vars = Map String Double

newtype Equation = Equation { with :: Vars -> (Double,Vars) }

instance IsString Equation where
  fromString = var

var :: String -> Equation
var x = Equation $ \vars ->
  ( Map.findWithDefault 0 x vars
  , Map.singleton x 1
  )

instance Num Equation where
  a + b = Equation $ \vars ->
    let (ra, das) = a `with` vars
        (rb, dbs) = b `with` vars
    in (ra + rb, Map.unionWith (+) das dbs)
  a * b = Equation $ \vars ->
    let (ra, das) = a `with` vars
        (rb, dbs) = b `with` vars
    in (ra * rb, Map.unionWith (+) ((* rb) <$> das) ((* ra) <$> dbs))
  negate a = Equation $ \vars ->
    let (r, ds) = a `with` vars
    in (negate r, negate <$> ds)
  abs a = Equation $ \vars ->
    let (r, ds) = a `with` vars
    in (abs r, (* signum r) <$> ds)
  signum a = Equation $ \vars ->
    let (r, _) = a `with` vars
    in (signum r, Map.empty)
  fromInteger a = Equation $ \_ ->
    (fromInteger a, Map.empty)

instance Fractional Equation where
  a / b = Equation $ \vars ->
    let (ra, das) = a `with` vars
        (rb, dbs) = b `with` vars
    in ( ra / rb
       , (/ rb ^ 2)
         <$> Map.unionWith (+)
             ((* rb) <$> das)
             (negate . (* ra) <$> dbs)
       )
  fromRational a = Equation $ \_ ->
    (fromRational a, Map.empty)

epsilon :: Double
epsilon = 1e-12

step :: Equation -> Vars -> Maybe Vars
step eqn vars =
  if r < epsilon || all ((< epsilon) . abs) ds || converged
  then Nothing
  else Just next
  where
  (r, ds) = eqn `with` vars
  (next, converged) = backtrack 1
  threshold = 0.5 * sum ((^ 2) <$> ds)
  backtrack stepSize =
    if r - r' >= stepSize * threshold
    then (vars', abs (r - r') < epsilon)
    else backtrack $ stepSize * 0.5
    where
    vars' = Map.unionWith (-) vars
            $ (* stepSize) <$> ds
    r'    = fst $ eqn `with` vars'

minimize :: Equation -> Vars -> Vars
minimize eqn vars =
  Maybe.fromJust
  $ last
  $ takeWhile Maybe.isJust
  $ iterate (step eqn =<<) (return vars)

infixl 3 ===
(===) :: Equation -> Equation -> Equation
(===) = (-)

solveSystem :: [Equation] -> Vars -> ([Bool], Vars)
solveSystem eqns vars =
  if and satisfied
  then (satisfied, vars')
  else let index             = Maybe.fromJust $ List.elemIndex False satisfied
           (front, back)     = splitAt index eqns
           (satisfied', out) = solveSystem (front ++ drop 1 back) vars'
           (a, b)            = splitAt index satisfied'
       in (a ++ False : b, out)
  where
  vars'     = minimize (sum $ map (^ 2) eqns) vars
  scores    = (^ 2) . fst . (`with` vars') <$> eqns
  satisfied = (< sqrt epsilon) <$> scores

sys1 :: [Equation]
sys1 =
  [ "ax" ^ 2 + "ay" ^ 2 === 1
  , quad ("ax","ay") ("bx","by") === 2
  , "by" === 0
  , "cy" === 0
  , "cx" - "bx" === 1
  ]

quad :: Num a => (a,a) -> (a,a) -> a
quad a b = (fst a - fst b) ^ 2 + (snd a - snd b) ^ 2
-}









{-
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Builder
import Data.Monoid ((<>))
import Data.Char
import Text.Printf
import qualified Data.List as List

data RA = Rel | Abs deriving (Eq,Ord,Show)

type Path = [PathCmd]
data PathCmd
  = MoveTo  RA R2
  | LineTo  RA R2
  | CurveTo RA R2 R2 R2
  | ArcTo   RA R R D Bool Bool R2
  | Close
  deriving (Eq,Ord,Show)

type    R  = Double
type    R2 = (R,R)
newtype D  = D Double
  deriving (Show)

deg :: R -> D
deg = D . (`mod'` 360)

getDegrees :: D -> R
getDegrees (D d) = d

instance Eq D where
  D x == D y = mod' x 360 ~= mod' y 360

instance Ord D where
  compare (D x) (D y) = compare x y

instance Num D where
  D x + D y      = deg $ x + y
  D x - D y      = deg $ x - y
  negate (D x)   = deg $ negate x
  abs (D x)      = deg $ abs x
  signum (D x)   = deg $ signum x
  fromInteger x  = deg $ fromInteger x
  (*)            = undefined

instance Fractional D where
  (/)            = undefined
  recip          = undefined
  fromRational x = deg $ fromRational x

mod' :: (Ord n, Num n) => n -> n -> n
mod' n d
  | n < 0 = mod' (n + d) d
  | n > d = mod' (n - d) d
  | otherwise = n

type Ellipse = (R2,R2)

-- PathCmd shorthand {{{

_m, _M :: R2 -> PathCmd
_m = MoveTo Rel
_M = MoveTo Abs

_l, _L :: R2 -> PathCmd
_l = LineTo Rel
_L = LineTo Abs

_h, _H, _v, _V :: R -> PathCmd
_h = _l . (,0)
_H = _L . (,0)
_v = _l . (0,)
_V = _L . (0,)

_c, _C :: R2 -> R2 -> R2 -> PathCmd
_c = CurveTo Rel
_C = CurveTo Abs

_a, _A :: R -> R -> D -> Bool -> Bool -> R2 -> PathCmd
_a = ArcTo Rel
_A = ArcTo Abs

_z :: PathCmd
_z = Close

-- }}}

-- Example path constructors {{{

rectanglePath :: R2 -> R -> R -> Path
rectanglePath (cx,cy) w h =
  [ _m (cx - w / 2 , cy - h / 2)
  , _h w
  , _v h
  , _h (-w)
  , _z
  ]

-- }}}

-- Rendering {{{

buildPath :: Path -> Builder
buildPath = buildWords . fmap buildCmd

buildCmd :: PathCmd -> Builder
buildCmd = buildWords . \case
  MoveTo ra p ->
    [ charRA 'm' ra
    , buildR2 p
    ]
  LineTo ra p ->
    [ charRA 'l' ra
    , buildR2 p
    ]
  CurveTo ra p1 p2 p ->
    [ charRA 'c' ra
    , buildR2 p1
    , buildR2 p2
    , buildR2 p
    ]
  ArcTo ra rx ry xrot la sw p ->
    [ charRA 'a' ra
    , buildR rx
    , buildR ry
    , buildD xrot
    , buildFlag la
    , buildFlag sw
    , buildR2 p
    ]
  Close ->
    [ char 'z'
    ]

charRA :: Char -> RA -> Builder
charRA c ra = char $ withRA ra c

withRA :: RA -> Char -> Char
withRA = \case
  Rel -> toLower
  Abs -> toUpper

buildR2 :: R2 -> Builder
buildR2 (x,y) = buildR x <> char ',' <> buildR y

buildR :: R -> Builder
buildR =
  string
  . List.dropWhileEnd (== '.') -- drops decimal point if number has no fractional part
  . List.dropWhileEnd (== '0') -- drops superfluous 0's
  . printf "%.5f"              -- prints with precision of *exactly* 5

buildD :: D -> Builder
buildD = buildR . getDegrees

buildFlag :: Bool -> Builder
buildFlag = buildR . cond 1 0

buildWords :: Foldable f => f Builder -> Builder
buildWords = intercalate $ char ' '

-- }}}

-- Util {{{

nearZero :: Double -> Bool
nearZero = (< 1e-12)

(~=) :: Double -> Double -> Bool
x ~= y = nearZero $ abs $ x - y
infix 4 ~=

cond :: a -> a -> Bool -> a
cond t f b = if b then t else f

-- }}}
-}

{-
m 666.3432,573.55757
a 105,154.65913 35.36184 0 1 -175.13604,65.35908
  105,154.65913 35.36184 0 1 3.87822,-186.8941
  105,154.65913 35.36184 0 1 175.13604,-65.35908
  105,154.65913 35.36184 0 1 -3.87822,186.8941
z
-}

{-
m 685.71429,512.79006
a 105,154.65913 0 0 1 -105,154.65912
  105,154.65913 0 0 1 -105,-154.65912
  105,154.65913 0 0 1 105,-154.65912
  105,154.65913 0 0 1 105,154.65912
z
-}

{-
svg-path:
    wsp* moveto-drawto-command-groups? wsp*
moveto-drawto-command-groups:
    moveto-drawto-command-group
    | moveto-drawto-command-group wsp* moveto-drawto-command-groups
moveto-drawto-command-group:
    moveto wsp* drawto-commands?
drawto-commands:
    drawto-command
    | drawto-command wsp* drawto-commands
drawto-command:
    closepath
    | lineto
    | horizontal-lineto
    | vertical-lineto
    | curveto
    | smooth-curveto
    | quadratic-bezier-curveto
    | smooth-quadratic-bezier-curveto
    | elliptical-arc
moveto:
    ( "M" | "m" ) wsp* moveto-argument-sequence
moveto-argument-sequence:
    coordinate-pair
    | coordinate-pair comma-wsp? lineto-argument-sequence
closepath:
    ("Z" | "z")
lineto:
    ( "L" | "l" ) wsp* lineto-argument-sequence
lineto-argument-sequence:
    coordinate-pair
    | coordinate-pair comma-wsp? lineto-argument-sequence
horizontal-lineto:
    ( "H" | "h" ) wsp* horizontal-lineto-argument-sequence
horizontal-lineto-argument-sequence:
    coordinate
    | coordinate comma-wsp? horizontal-lineto-argument-sequence
vertical-lineto:
    ( "V" | "v" ) wsp* vertical-lineto-argument-sequence
vertical-lineto-argument-sequence:
    coordinate
    | coordinate comma-wsp? vertical-lineto-argument-sequence
curveto:
    ( "C" | "c" ) wsp* curveto-argument-sequence
curveto-argument-sequence:
    curveto-argument
    | curveto-argument comma-wsp? curveto-argument-sequence
curveto-argument:
    coordinate-pair comma-wsp? coordinate-pair comma-wsp? coordinate-pair
smooth-curveto:
    ( "S" | "s" ) wsp* smooth-curveto-argument-sequence
smooth-curveto-argument-sequence:
    smooth-curveto-argument
    | smooth-curveto-argument comma-wsp? smooth-curveto-argument-sequence
smooth-curveto-argument:
    coordinate-pair comma-wsp? coordinate-pair
quadratic-bezier-curveto:
    ( "Q" | "q" ) wsp* quadratic-bezier-curveto-argument-sequence
quadratic-bezier-curveto-argument-sequence:
    quadratic-bezier-curveto-argument
    | quadratic-bezier-curveto-argument comma-wsp? 
        quadratic-bezier-curveto-argument-sequence
quadratic-bezier-curveto-argument:
    coordinate-pair comma-wsp? coordinate-pair
smooth-quadratic-bezier-curveto:
    ( "T" | "t" ) wsp* smooth-quadratic-bezier-curveto-argument-sequence
smooth-quadratic-bezier-curveto-argument-sequence:
    coordinate-pair
    | coordinate-pair comma-wsp? smooth-quadratic-bezier-curveto-argument-sequence
elliptical-arc:
    ( "A" | "a" ) wsp* elliptical-arc-argument-sequence
elliptical-arc-argument-sequence:
    elliptical-arc-argument
    | elliptical-arc-argument comma-wsp? elliptical-arc-argument-sequence
elliptical-arc-argument:
    nonnegative-number comma-wsp? nonnegative-number comma-wsp? 
        number comma-wsp flag comma-wsp? flag comma-wsp? coordinate-pair
coordinate-pair:
    coordinate comma-wsp? coordinate
coordinate:
    number
nonnegative-number:
    integer-constant
    | floating-point-constant
number:
    sign? integer-constant
    | sign? floating-point-constant
flag:
    "0" | "1"
comma-wsp:
    (wsp+ comma? wsp*) | (comma wsp*)
comma:
    ","
integer-constant:
    digit-sequence
floating-point-constant:
    fractional-constant exponent?
    | digit-sequence exponent
fractional-constant:
    digit-sequence? "." digit-sequence
    | digit-sequence "."
exponent:
    ( "e" | "E" ) sign? digit-sequence
sign:
    "+" | "-"
digit-sequence:
    digit
    | digit digit-sequence
digit:
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
wsp:
    (#x20 | #x9 | #xD | #xA)
-}


