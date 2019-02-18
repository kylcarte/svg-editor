
module Main where

import Interface
import Spec

import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as Map
import qualified Data.Time as Time

main :: IO ()
main = do
  logPath <- newLogFile
  putStrLn $ "writing log to file " ++ logPath ++ "\n"
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

newLogFile :: IO String
newLogFile = do
  time <- Time.getCurrentTime
  let path = concat
        [ "log/"
        , Time.formatTime Time.defaultTimeLocale "%s" time
        , ".log"
        ]
  writeFile path ""
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

