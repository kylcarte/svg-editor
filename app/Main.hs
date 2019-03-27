
module Main where

import Editor
import Interface
import Spec

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map
import qualified Data.Time as Time
import Control.Monad (when)

main :: IO ()
main = do
  ts <- timestamp
  logPath <- makeLogFile ts
  putStrLn $ "writing log to file " ++ logPath ++ "\n"
  -- optLogPath <- makeLogFile $ ts ++ "-opt"
  -- putStrLn $ "writing optimization details to file " ++ optLogPath ++ "\n"
  -- let paths = LogPaths logPath logPath

  logModel logPath initState

  playIO
    (InWindow "svg-editor" windowSize windowPos)
    white
    60
    initState
    (drawEditor windowScale)
    (handleEvent logPath windowScale)
    stepEditor
  where
  windowScale :: Num a => a
  windowScale = 3
  windowSize :: Num a => (a,a)
  windowSize = (600,600)
  windowPos :: Num a => (a,a)
  windowPos = (50,50)

timestamp :: IO String
timestamp = do
  time <- Time.getCurrentTime
  return $ Time.formatTime Time.defaultTimeLocale "%s" time

makeLogFile :: String -> IO FilePath
makeLogFile name = do
  let path = concat
        [ "log/"
        , name
        , ".log"
        ]
  when shouldLog $ writeFile path "" -- XXX: global defined in Editor.hs
  return path

initState :: Editor
initState =
  initEditor
  rotatableRectangle
  $ Map.fromList
    [ ( "w"     , 90 )
    , ( "h"     , 30 )
    , ( "cx"    , 0 )
    , ( "cy"    , 0 )
    , ( "theta" , (-pi) / 5 )
    ]

