
module Interface where

import Spec
import Doc

import Graphics.Gloss.Interface.Pure.Game hiding (Path)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss

data EdState = EdState
  { edSpec :: Spec
  , edDoc  :: Doc
  } deriving (Eq,Ord,Show)

edRender :: EdState -> Picture
edRender = undefined

edHandle :: Event -> EdState -> EdState
edHandle = undefined

edTick :: Float -> EdState -> EdState
edTick = undefined

