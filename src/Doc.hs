
module Doc where

import Spec
import Data.Map (Map)
import qualified Data.Map as Map

data Val
  = V_R Double
  deriving (Eq,Ord,Show)

type ShapeData = Map String Val

type Doc = Map String ShapeData

