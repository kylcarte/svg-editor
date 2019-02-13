{-# LANGUAGE LambdaCase #-}

module Interface where

import Spec
import Expr
import Path

import Graphics.Gloss.Interface.Pure.Game hiding (Path)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Data.Maybe as Maybe
import Control.Arrow ((***))
import Control.Monad.Trans.RWS
import Data.Monoid (First(..))

type R = Float

data EdState = EdState
  { edSpec :: ShapeType -- Spec, but single shape definition
  , edDoc  :: Env R     -- Doc, but single shape instance
  } deriving (Eq,Ord,Show)
-- Float or Double?

-- Rendering {{{

edRender :: (Float,Float) -> EdState -> Picture
edRender (w,h) s =
  translate (-w/2) (-h/2)
  $ scale 2 2
  $ case traverse (evalWithShape st env) path of
      Left err -> renderMsg $ show err
      Right p  -> renderPath p
  where
  st   = edSpec s
  path = shapeRender st
  env  = edDoc s

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

{-
  mappend
  ( case (isClosed,p0) of
      (True,First (Just start)) ->
        line [pt end,pt start]
      _ -> mempty
  ) p
  where
  (_,end,(p0,p)) = runRWS (mapM_ execCmd cs) () (0,0)
-}

{-
execCmd :: Cmd R -> RWS () (First (R,R),Picture) (R,R) ()
  -- (first line head, cumulative picture), cursor
execCmd = \case
  MoveTo isAbs x' y' ->
    modify $ move isAbs x' y'
  LineTo isAbs x' y' -> do
    c <- get
    let c' = move isAbs x' y' c
    tell ( First $ Just c
         , line [pt c,pt c']
         )
    put c'
  where
  move :: Bool -> R -> R -> (R,R) -> (R,R)
  move isAbs x' y'
    | isAbs = const x' *** const y'
    | otherwise = (x' +) *** (y' +)
-}

{-
renderPath :: Path R -> Picture
renderPath (Path isClosed cs) =
  renderMsg $ unlines
    [ "closed: " ++ show isClosed
    , show (s,ss)
    ]
  -- if isClosed && null ss
  -- then mkPolygon s
  -- else mkLines $ s0 : ss
  where
  (s,ss) = splitSegments cs
  s0 = ((True,0,0),s)
-}

{-
segmentText :: [Point] -> Picture
segmentText = _
-}

mkPolygon :: [(Bool,R,R)] -> Picture
mkPolygon s =
  polygon
  -- segmentText
  $ snd
  $ execRWS (processSegment s) () (0,0)

mkLines :: [((Bool,R,R),[(Bool,R,R)])] -> Picture
mkLines ss =
  foldMap line
  -- foldr
  --   ( \(i,ps) ->
  --     mappend
  --     $ translate 0 (i * l)
  --     $ segmentText ps
  --   ) mempty
  -- $ zip [0..]
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

