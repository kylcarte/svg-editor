{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Spec where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Path
import Expr

type Spec = Env ShapeType

data Ty
  = R
  deriving (Eq,Ord,Show)

data ShapeType = ShapeType
  { shapeParams   :: Env Ty
  , shapeDefs     :: Env Expr
  , shapeRender   :: Path Expr
  , shapeHandles  :: Env (Expr,Expr)
  , shapeControls :: Env FixedVals
  } deriving (Eq,Ord,Show)

shapeBindings :: ShapeType -> Env Expr
shapeBindings st =
  (Map.mapWithKey (const . Var) $ shapeParams st)
  <> shapeDefs st

handleFixList :: ShapeType -> String -> [Expr]
handleFixList st h =
  foldMap
  ( foldMap $ \case
    Var h' | Just (hx',hy') <- Map.lookup h' (shapeHandles st)
      -> [hx',hy']
    e -> [e]
  ) fvs
  where
  fvs = Map.lookup h $ shapeControls st

type FixedVals = Set Expr

data Transform
  = Move
  deriving (Eq,Ord,Show)

rectangle :: ShapeType
rectangle = ShapeType
  { shapeParams   = [ ( "w"  , R )
                    , ( "h"  , R )
                    , ( "cx" , R )
                    , ( "cy" , R )
                    ]
  , shapeDefs     = [ ( "left"   , - "w" / 2 )
                    , ( "right"  , "w" / 2 )
                    , ( "top"    , - "h" / 2 )
                    , ( "bottom" , "h" / 2 )
                    ]
  , shapeRender   = [ _M "cx" "cy"
                    , _m "left" "top"
                    , _l "w" 0
                    , _l 0 "h"
                    , _l (- "w") 0
                    , _z
                    ]
  , shapeHandles  = [ ( "lt" , ("cx" + "left"  , "cy" + "top") )
                    , ( "rt" , ("cx" + "right" , "cy" + "top") )
                    , ( "lb" , ("cx" + "left"  , "cy" + "bottom") )
                    , ( "rb" , ("cx" + "right" , "cy" + "bottom") )
                    ]
  , shapeControls = [ ( "lt" , [ "rb" ] )
                    , ( "rt" , [ "lb" ] )
                    , ( "lb" , [ "rt" ] )
                    , ( "rb" , [ "lt" ] )
                    ]
  }

rotatableRectangle :: ShapeType
rotatableRectangle = ShapeType
  { shapeParams   = [ ( "w"     , R )
                    , ( "h"     , R )
                    , ( "cx"    , R )
                    , ( "cy"    , R )
                    , ( "theta" , R )
                    ]
  , shapeDefs     = [ ( "left"   , - "w" / 2 )
                    , ( "right"  , "w" / 2 )
                    , ( "top"    , - "h" / 2 )
                    , ( "bottom" , "h" / 2 )
                    ]
  , shapeRender   = [ _M "cx" "cy"
                    , _m ("left" * cos "theta" - "top" * sin "theta")
                         ("left" * sin "theta" + "top" * cos "theta")
                    , _l ("w" * cos "theta") ("w" * sin "theta")
                    , _l (- "h" * sin "theta") ("h" * cos "theta")
                    , _l (- "w" * cos "theta") (- "w" * sin "theta")
                    , _z
                    ]
  , shapeHandles  = [ ( "lt" , ( "cx" + "left" * cos "theta" - "top" * sin "theta"
                               , "cy" + "left" * sin "theta" + "top" * cos "theta"
                               )
                      )
                    , ( "rt" , ( "cx" + "right" * cos "theta" - "top" * sin "theta"
                               , "cy" + "right" * sin "theta" + "top" * cos "theta"
                               )
                      )
                    , ( "lb" , ( "cx" + "left" * cos "theta" - "bottom" * sin "theta" 
                               , "cy" + "left" * sin "theta" + "bottom" * cos "theta" 
                               )
                      )
                    , ( "rb" , ( "cx" + "right" * cos "theta" - "bottom" * sin "theta" 
                               , "cy" + "right" * sin "theta" + "bottom" * cos "theta" 
                               )
                      )
                    , ( "r"  , ( "cx" + 10 * cos "theta"
                               , "cy" + 10 * sin "theta"
                               )
                      )
                    ]
  , shapeControls = [ ( "lt" , [ "rb", "theta" ] )
                    , ( "rt" , [ "lb", "theta" ] )
                    , ( "lb" , [ "rt", "theta" ] )
                    , ( "rb" , [ "lt", "theta" ] )
                    , ( "r"  , [ "w", "h", "cx", "cy" ] )
                    ]
  }

