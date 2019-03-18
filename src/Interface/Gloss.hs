{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Interface.Gloss where

import Editor

import Expr
import Path
import Spec

import Opt
import Problem

import Graphics.Gloss.Interface.IO.Game hiding (Path)
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Control.Monad.Trans.RWS

-- Rendering {{{

drawEditor :: Float -> Editor -> IO Picture
drawEditor sc ed =
  return
  $ scale sc sc
  $ pictures
  [ drawShape st env
  , drawHandles st env
  -- , drawCursor curs
  ]
  where
  st     = edSpec ed
  env    = edDoc ed
  -- curs   = edCursor ed
  -- active = edActive ed

drawShape :: ShapeType -> Env Float -> Picture
drawShape st env =
  drawEval renderPath
  $ traverse (evalWithShape st env)
  $ shapeRender st

drawEval :: (a -> Picture) -> Either EvalErr a -> Picture
drawEval = either (renderMsg . show)

drawHandles :: ShapeType -> Env Float -> Picture
drawHandles st env =
  drawEval
  ( foldMap drawHandle
  ) $ evalHandles st env

drawHandle :: Point -> Picture
drawHandle p =
  uncurry translate p
  $ color hColor
  $ circle cursorRadius
  where
  hColor = greyN 0.5

{-
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
-}

{-
drawCursor :: Point -> Picture
drawCursor curs =
  uncurry translate curs
  $ color
    ( blendColors blue green
    )
  $ circle cursorRadius
-}

renderMsg :: String -> Picture
renderMsg msg =
  translate 10 (-20)
  $ scale s s
  $ color red
  $ text msg
  where
  s = 0.15

accumPicture :: Bool -> Point -> ([Point] -> [Point]) -> Picture -> Picture
accumPicture close c acc p =
  if length ps < 2
  then p
  else mappend p $
    if close
    then polygon ps
    else line ps
  where
  ps = acc [c]

renderPath :: Path Float -> Picture
renderPath (Path close cs) =
  accumPicture close c acc p
  where
  (c,acc,p) = foldl (execCmd close) ((0,0),id,mempty) cs

execCmd :: Bool
  -> (Point,([Point] -> [Point]),Picture)
  -> Cmd Float
  -> (Point,([Point] -> [Point]),Picture)
execCmd close (c,acc,p) = \case
  MoveTo isAbs x' y' ->
    ( movePt isAbs (x',y') c
    , id
    , accumPicture close c acc p
    )
  LineTo isAbs x' y' ->
    ( movePt isAbs (x',y') c
    , (c:) . acc
    , p
    )

type Move = (Bool,Vector) -- Abs/Rel, movement

mkPolygon :: [Move] -> Picture
mkPolygon s =
  polygon
  $ snd
  $ execRWS (processSegment s) () (0,0)

mkLines :: [(Move,[Move])] -> Picture
mkLines ss =
  foldMap line
  $ snd
  $ execRWS (processLines ss) () (0,0)
  where
  l :: Float
  l = -10

processLines :: [(Move,[Move])] -> RWS () [[Point]] Point ()
processLines = mapM_ $ \((isAbs,p'),s) -> do
  modify $ if isAbs
    then const p'
    else addPt p'
  mapRWS (onThd (:[]))
    $ processSegment s
  where
  onThd f (x,y,z) = (x,y,f z)

processSegment :: [Move] -> RWS () [Point] Point ()
processSegment = mapM_ $ \(isAbs,p') -> do
  get >>= tell . (:[])
  modify $ movePt isAbs p'

-- }}}

-- Events {{{

handleEvent :: FilePath -> Float -> Event -> Editor -> IO Editor
handleEvent logPath sc ev ed
  -- mouse movement
  | EventMotion (unscalePt sc -> curs) <- ev
  , Just h <- dragging
  = do let (itrace,env') = moveHandle st env h curs
       logDrag logPath h curs
       logOpt logPath itrace
       return $! ed
         { edDoc = env'
         }

  | EventKey (SpecialKey KeyEnter) Down _ _ <- ev
  = do logModel logPath ed
       return ed

  -- handle dragging
  | EventKey (MouseButton LeftButton) Down _ (unscalePt sc -> curs) <- ev
  , Right hs <- evalHandles st env
  , Just h <- nearestWithin cursorRadius curs hs
  = do logClick logPath h curs
       return $! ed
         { edDrag = Just h }
  | EventKey (MouseButton LeftButton) Up _ (unscalePt sc -> curs) <- ev
  , Just h <- dragging
  = do logRelease logPath h curs
       return $! ed
         { edDrag = Nothing }

  | otherwise
  = return ed
  where
  st       = edSpec ed
  env      = edDoc ed
  dragging = edDrag ed

{-
writeLog :: FilePath -> String -> IO ()
writeLog logPath ((++ "\n") -> msg) = do
  putStr msg
  appendFile logPath msg

logWords :: FilePath -> [String] -> IO ()
logWords p = writeLog p . unwords

logLines :: FilePath -> [String] -> IO ()
logLines p = writeLog p . unlines
-}

-- }}}

-- Timestep {{{

stepEditor :: Float -> Editor -> IO Editor
stepEditor = const return

{-
  | Just (h,curs) <- edDrag ed
  = do -- putStrLn $ "dragging handle " ++ show h
       let (env',info) = moveHandle st env h curs
       -- putStrLn info
       return ed
         { edDoc = env'
         }
  | otherwise
  = return ed
  where
  st  = edSpec ed
  env = edDoc ed
-}

-- }}}

-- Color Utils {{{

blendColors :: Color -> Color -> Color
blendColors = mixColors 0.5 0.5

-- }}}

