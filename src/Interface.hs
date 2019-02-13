{-# LANGUAGE LambdaCase #-}

module Interface where

import Spec
import Doc
import Expr
import Path

import Graphics.Gloss.Interface.Pure.Game hiding (Path)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Data.Maybe as Maybe
import Control.Arrow ((***))
import Control.Monad.Trans.RWS

type R = Float

data EdState = EdState
  { edSpec :: ShapeType -- Spec, but single shape definition
  , edDoc  :: Env R     -- Doc, but single shape instance
  } deriving (Eq,Ord,Show)
-- Float or Double?

-- Rendering {{{

edRender :: EdState -> Picture
edRender s = case traverse (eval env) path of
  Left err -> color red $ text $ show err
  Right p  -> renderPath p
  where
  path = shapeRender $ edSpec s
  env = edDoc s

renderPath :: Path R -> Picture
renderPath (Path isClosed cs) =
  if isClosed && null ss
  then mkPolygon s
  else mkLines $ s0 : ss
  where
  (s,ss) = splitSegments cs
  s0 = ((True,0,0),s)

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

splitSegments :: [Cmd R] -> ([(Bool,R,R)],[((Bool,R,R),[(Bool,R,R)])])
splitSegments = splitEither $ \case
  LineTo isAbs x' y' -> Right (isAbs,x',y')
  MoveTo isAbs x' y' -> Left  (isAbs,x',y')

splitEither :: (a -> Either b c) -> [a] -> ([c],[(b,[c])])
splitEither f =
  foldr
  (\a (cs,rest) ->
    case f a of
      Left b  -> ([], (b,cs) : rest)
      Right c -> (c : cs, rest)
  )
  ([],[])

-- }}}

edHandle :: Event -> EdState -> EdState
edHandle = const id

edTick :: Float -> EdState -> EdState
edTick = const id

