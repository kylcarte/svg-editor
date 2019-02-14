
module Main where

import Interface
import Spec

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as Map

main :: IO ()
main = play
  (InWindow "svg-editor" windowSize windowPos)
  white
  60
  initState
  drawEditor
  handleEvent
  stepEditor
  where
  windowSize :: Num a => (a,a)
  windowSize = (600,600)
  windowPos :: Num a => (a,a)
  windowPos = (50,50)

initState :: Editor
initState =
  scaleEditor 3
  $ initEditor
    rotatableRectangle
    $ Map.fromList
      [ ( "w"     , 90 )
      , ( "h"     , 30 )
      , ( "cx"    , 0 )
      , ( "cy"    , 0 )
      , ( "theta" , (-pi) / 5 )
      ]

