{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Spec where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid ((<>))
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

type Handle = String
type FixedVals = [Expr]

evalWithShape :: (Floating a, Ord a)
  => ShapeType -> Env a -> Expr -> Either EvalErr a
evalWithShape st env e = do
  env' <- traverse (eval env) $ shapeDefs st
  eval (env <> env') e

handleFixList :: ShapeType -> Handle -> [Expr]
handleFixList st h =
  foldMap
    ( foldMap $ \case
      Var x | Just (hx,hy) <- Map.lookup x $ shapeHandles st
           -> [hx,hy]
      e    -> [e]
    )
  $ Map.lookup h
  $ shapeControls st

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
  , shapeRender   = closedPath
                    [ _M "cx" "cy"
                    , _m "left" "top"
                    , _l "w" 0
                    , _l 0 "h"
                    , _l (- "w") 0
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
  , shapeRender   = closedPath
                    [ _M "cx" "cy"
                    , _m ("left" * cos "theta" - "top" * sin "theta")
                         ("left" * sin "theta" + "top" * cos "theta")
                    , _l ("w" * cos "theta") ("w" * sin "theta")
                    , _l (- "h" * sin "theta") ("h" * cos "theta")
                    , _l (- "w" * cos "theta") (- "w" * sin "theta")
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
                    , ( "r"  , ( "cx" + 30 * cos "theta"
                               , "cy" + 30 * sin "theta"
                               )
                      )
                    , ( "c"  , ( "cx"
                               , "cy"
                               )
                      )
                    ]
  , shapeControls = [ ( "lt" , [ "rb", "theta" ] )
                    , ( "rt" , [ "lb", "theta" ] )
                    , ( "lb" , [ "rt", "theta" ] )
                    , ( "rb" , [ "lt", "theta" ] )
                    , ( "r"  , [ "w", "h", "cx", "cy" ] )
                    , ( "c"  , [ "w", "h", "theta" ] )
                    ]
  }

