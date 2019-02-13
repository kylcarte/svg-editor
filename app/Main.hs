
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
  (edRender windowSize)
  edHandle
  edTick
  where
  windowSize :: Num a => (a,a)
  windowSize = (400,400)
  windowPos :: Num a => (a,a)
  windowPos = (50,50)

initState :: EdState
initState = EdState
  { edSpec = rotatableRectangle
  , edDoc  = Map.fromList
    [ ( "w"     , 50 )
    , ( "h"     , 30 )
    , ( "cx"    , 50 )
    , ( "cy"    , 50 )
    , ( "theta" , pi / 6 )
    ]
  }

