{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Editor where

import Expr
import Spec

import Opt
import Control.Arrow ((***))
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map

data Editor = Editor
  { edSpec   :: ShapeType      -- Spec, but single shape definition
  , edDoc    :: Env Float      -- Doc, but single shape instance
  , edDrag   :: Maybe Handle   -- handle currently being dragged, if any
  } deriving (Show)
-- R as Float or Double?
-- * gloss uses Float

{-
data LogPaths = LogPaths
  { genLog :: FilePath
  , optLog :: FilePath
  } deriving (Eq,Ord,Show)
-}

ppEditor :: Editor -> String
ppEditor ed = unwords
  [ showsPrec 11 drag ""
  , ppDoc doc
  ]
  where
  doc  = edDoc ed
  drag = edDrag ed

ppDoc :: Env Float -> String
ppDoc = show . Map.toList

shouldLog :: Bool
shouldLog = True

shouldPrintLog :: Bool
shouldPrintLog = True

-- Editor defaults {{{

initEditor :: ShapeType -> Env Float -> Editor
initEditor st env = Editor
  { edSpec   = st
  , edDoc    = env
  , edDrag   = Nothing
  }

cursorRadius :: Float
cursorRadius = 2

-- }}}

-- Logging {{{

logClick, logDrag, logRelease :: FilePath -> Handle -> (Float,Float) -> IO ()
logClick   = logHandleMsg "click"
logDrag    = logHandleMsg "drag"
logRelease = logHandleMsg "release"

logHandleMsg :: String -> FilePath -> Handle -> (Float,Float) -> IO ()
logHandleMsg msg logPath h p =
  logWords logPath
    [ msg , h , show $ fst p , show $ snd p ]

logModel :: FilePath -> Editor -> IO ()
logModel logPath ed =
  logWords logPath
    [ "model" , ppEditor ed ]

logOpt :: FilePath -> IterTrace -> IO ()
logOpt logPath itrace = do
  logWords logPath
    [ "opt"
    , show $ length itrace
    , "len"
    ]
  logLines logPath $ foldMap
    ( \(w,lsi,st) ->
      [ unwords $ foldMap (pure . show) lsi
      , "penalty: " ++ show w
      , show $ Map.toList st
      ]
    ) itrace
      -- foldr
      -- ( (:) . (_
      -- -- maybe (show lsLimit) show
      -- ) [] itrace
  where
  dummyParams = initParams $ error "dummy objective function"
  epLimit = epIterLimit dummyParams
  lsLimit = lsIterLimit dummyParams

writeLog :: FilePath -> String -> IO ()
writeLog logPath ((++ "\n") -> msg) =
  when shouldLog $ do
  when shouldPrintLog $ putStr msg
  appendFile logPath msg

logWords :: FilePath -> [String] -> IO ()
logWords p = writeLog p . unwords

logLines :: FilePath -> [String] -> IO ()
logLines p = writeLog p . unlines

-- }}}

-- Handle Utils {{{

evalHandles :: ShapeType -> Env Float -> Either EvalErr (Env (Float,Float))
evalHandles st env =
  traverse (\(x,y) -> (,) <$> ev x <*> ev y)
  $ shapeHandles st
  where
  ev = evalWithShape st env

{-
activeHandles :: (Float,Float) -> ShapeType -> Env Float -> ActiveHandles
activeHandles curs st env =
  either (const Seq.empty)
    ( Map.foldMapWithKey $ \h p ->
      if inRadius cursorRadius curs p
      then Seq.singleton h
      else Seq.empty
    )
  $ evalHandles st env
-}

{-
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
-}

-- }}}

-- Point Utils {{{

scalePt :: Float -> (Float,Float) -> (Float,Float)
scalePt s = (s *) *** (s *)

unscalePt :: Float -> (Float,Float) -> (Float,Float)
unscalePt = scalePt . recip

addPt :: (Float,Float) -> (Float,Float) -> (Float,Float)
addPt (x,y) = (x +) *** (y +)

negPt :: (Float,Float) -> (Float,Float)
negPt = negate *** negate

subPt :: (Float,Float) -> (Float,Float) -> (Float,Float)
subPt p = addPt p . negPt

normSqrPt :: (Float,Float) -> Float
normSqrPt (x,y) = x ^ 2 + y ^ 2

movePt :: Bool -> (Float,Float) -> (Float,Float) -> (Float,Float)
movePt isAbs p
  | isAbs = const p
  | otherwise = addPt p

{-
inRadius :: Float -> (Float,Float) -> (Float,Float) -> Bool
inRadius r (x,y) (x',y') =
  r ^ 2 >= (x' - x) ^ 2 + (y' - y) ^ 2
-}

nearestWithin :: Float -> (Float,Float) -> Env (Float,Float) -> Maybe String
nearestWithin r p =
  fmap fst
  . minimumByWithKey (const id)
  . Map.filter (<= r ^ 2)
  . fmap (normSqrPt . subPt p)

minimumByWithKey :: Ord b => (k -> a -> b) -> Map k a -> Maybe (k,b)
minimumByWithKey f =
  Map.foldlWithKey
  ( \mkb k a ->
    let b = f k a
        p = (k,b)
    in
    maybe (Just p)
    ( \p'@(_,b') -> Just
      $ if b' > b
        then p
        else p'
    ) mkb
  )
  Nothing

-- }}}

