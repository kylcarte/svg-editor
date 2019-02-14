{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Interface where

import Expr
import Opt
import Path
import Spec

import Graphics.Gloss.Interface.Pure.Game hiding (Path)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Data.Maybe as Maybe
import Control.Arrow ((***))
import Control.Monad.Trans.RWS
import Data.Monoid (First(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq

type R = Float

data Editor = Editor
  { edSpec   :: ShapeType -- Spec, but single shape definition
  , edDoc    :: Env R     -- Doc, but single shape instance
  , edCursor :: Point
  , edScale  :: Float
  , edActive :: ActiveHandles -- Handle name
  , edOpt    :: Maybe Params
  }
-- R as Float or Double?
-- * gloss uses Float

type ActiveHandles = Seq String

-- Editor {{{

initEditor :: ShapeType -> Env R -> Editor
initEditor st env = Editor
  { edSpec   = st
  , edDoc    = env
  , edCursor = (0,0)
  , edScale  = 1
  , edActive = Seq.empty
  , edOpt    = Nothing
  }

scaleEditor :: Float -> Editor -> Editor
scaleEditor s e = e { edScale = s * edScale e }

cursorRadius :: Float
cursorRadius = 2

-- }}}

-- Rendering {{{

drawEditor :: Editor -> Picture
drawEditor ed =
  scale sc sc
  $ pictures
  [ drawShape st env
  , drawHandles active st env
  , drawCursor curs
  ]
  where
  st     = edSpec ed
  env    = edDoc ed
  curs   = edCursor ed
  sc     = edScale ed
  active = edActive ed

drawShape :: ShapeType -> Env R -> Picture
drawShape st env =
  drawEval renderPath
  $ traverse (evalWithShape st env)
  $ shapeRender st

drawHandles :: ActiveHandles -> ShapeType -> Env R -> Picture
drawHandles active st env =
  drawEval
  ( Map.foldMapWithKey
  $ drawHandle active 
  ) $ evalHandles st env

drawEval :: (a -> Picture) -> Either EvalErr a -> Picture
drawEval = either (renderMsg . show)

drawHandle :: ActiveHandles -> String -> Point -> Picture
drawHandle active h p =
  uncurry translate p
  $ color hColor
  $ circle cursorRadius
  where
  hColor, cFocus, cActive, cInactive :: Color
  hColor = case Seq.viewl active of
    h' :< hs
      | h == h' 
      -> cFocus
      | any (h ==) hs
      -> cActive
    _ -> cInactive
  cFocus    = blendColors red orange
  cInactive = greyN 0.5
  cActive   = blendColors cFocus cInactive

drawCursor :: Point -> Picture
drawCursor curs =
  uncurry translate curs
  $ color
    ( blendColors blue green
    )
  $ circle cursorRadius

renderMsg :: String -> Picture
renderMsg msg =
  translate 10 (-20)
  $ scale s s
  $ color red
  $ text msg
  where
  s = 0.15

execCmd :: Bool
  -> ((R,R),([Point] -> [Point]),Picture)
  -> Cmd R
  -> ((R,R),([Point] -> [Point]),Picture)
execCmd close (c,acc,p) = \case
  MoveTo isAbs x' y' ->
    ( move isAbs x' y' c
    , id
    , accPicture close c acc p
    )
  LineTo isAbs x' y' ->
    ( move isAbs x' y' c
    , (c:) . acc
    , p
    )
  where
  move :: Bool -> R -> R -> (R,R) -> (R,R)
  move isAbs x' y'
    | isAbs = const x' *** const y'
    | otherwise = (x' +) *** (y' +)

accPicture :: Bool -> (R,R) -> ([Point] -> [Point]) -> Picture -> Picture
accPicture close c acc p =
  if length ps < 2
  then p
  else mappend p $
    if close
    then polygon ps
    else line ps
  where
  ps = acc [pt c]

renderPath :: Path R -> Picture
renderPath (Path close cs) =
  accPicture close c acc p
  where
  (c,acc,p) = foldl (execCmd close) ((0,0),id,mempty) cs


mkPolygon :: [(Bool,R,R)] -> Picture
mkPolygon s =
  polygon
  $ snd
  $ execRWS (processSegment s) () (0,0)

mkLines :: [((Bool,R,R),[(Bool,R,R)])] -> Picture
mkLines ss =
  foldMap line
  $ snd
  $ execRWS (processLines ss) () (0,0)
  where
  l :: Float
  l = -10

processLines :: [((Bool,R,R),[(Bool,R,R)])] -> RWS () [[Point]] (R,R) ()
processLines = mapM_ $ \((isAbs,x',y'),s) -> do
  modify $ if isAbs
    then const x' *** const y'
    else (+ x') *** (+ y')
  mapRWS (onThd (:[]))
    $ processSegment s
  where
  onThd f (x,y,z) = (x,y,f z)

processSegment :: [(Bool,R,R)] -> RWS () [Point] (R,R) ()
processSegment = mapM_ $ \(isAbs,x',y') -> do
  get >>= \o -> tell [pt o]
  modify $ if isAbs
    then const x' *** const y'
    else (+ x') *** (+ y')

pt :: Real a => (a,a) -> Point
pt = realToFrac *** realToFrac

-- }}}

-- Handles {{{

{-
drawHandles :: Point -> ShapeType -> Env R -> Picture
drawHandles curs st env =
  drawEval st env
    (\ev (x,y) -> (,) <$> ev x <*> ev y)
    ( foldMap
    $ drawHandle
    $ inRadius cursorRadius curs
    )
  $ shapeHandles st

drawHandle :: (Point -> Bool) -> Point -> Picture
drawHandle active p =
  uncurry translate p
  $ color 
    ( if active p
      then addColors red orange
      else greyN 0.5
    )
  $ circle cursorRadius
-}

evalHandles :: ShapeType -> Env R -> Either EvalErr (Env (R,R))
evalHandles st env =
  traverse (\(x,y) -> (,) <$> ev x <*> ev y)
  $ shapeHandles st
  where
  ev = evalWithShape st env

activeHandles :: Point -> ShapeType -> Env R -> Seq String
activeHandles curs st env =
  either (const Seq.empty)
    ( Map.foldMapWithKey $ \h p ->
      if inRadius cursorRadius curs p
      then Seq.singleton h
      else Seq.empty
    )
  $ evalHandles st env

seqFwd, seqBwd :: Seq a -> Seq a
seqFwd s
  | x :< xs <- Seq.viewl s
  = xs Seq.|> x
  | otherwise
  = Seq.empty

seqBwd s
  | xs :> x <- Seq.viewr s
  = x Seq.<| xs
  | otherwise
  = Seq.empty

-- }}}

handleEvent :: Event -> Editor -> Editor
handleEvent ev ed
  -- mouse movement
  | EventMotion (scalePt (recip sc) -> curs') <- ev
  = ed { edCursor = curs'
       , edActive = activeHandles curs' st env
       }

  -- selection cycling
  | EventKey (SpecialKey KeyUp) Down _ _ <- ev
  = ed { edActive = seqFwd active }
  | EventKey (SpecialKey KeyDown) Down _ _ <- ev
  = ed { edActive = seqBwd active }

  -- handle dragging
  | EventKey (MouseButton LeftButton) Down _ _ <- ev
  , h :< _ <- Seq.viewl active
  = ed -- TODO
  | EventKey (MouseButton LeftButton) Up _ _ <- ev
  = ed { edOpt = Nothing }

  | otherwise
  = ed
  where
  sc  = edScale ed
  st  = edSpec ed
  env = edDoc ed
  active = edActive ed
  opt    = edOpt ed

stepEditor :: Float -> Editor -> Editor
stepEditor = const id

scalePt :: Float -> Point -> Point
scalePt s (x,y) = (s * x, s * y)

inRadius :: Float -> Point -> Point -> Bool
inRadius r (x,y) (x',y') =
  r ^ 2 >= (x' - x) ^ 2 + (y' - y) ^ 2

blendColors :: Color -> Color -> Color
blendColors = mixColors 0.5 0.5

