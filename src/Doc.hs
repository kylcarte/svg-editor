
module Doc where

import Expr
import Spec
import Data.Map (Map)
import qualified Data.Map as Map

type Doc = Map Int ShapeVal

data ShapeVal = ShapeVal
  { shapeType :: String
  , shapeVal  :: Env Double
  } deriving (Eq,Ord,Show)

