
module Doc where

import Expr
import Spec
import Data.Map (Map)
import qualified Data.Map as Map

type ShapeName  = String
type Doc        = Map InstanceID ShapeVal
type InstanceID = Int

data ShapeVal = ShapeVal
  { shapeType :: String
  , shapeVal  :: Env Double
  } deriving (Eq,Ord,Show)

