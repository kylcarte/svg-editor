{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Path where

data Cmd a
  = MoveTo Bool a a
  | LineTo Bool a a
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Path a
  = Path Bool [Cmd a]
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)
-- type Path a = [Cmd a]

closedPath, openPath :: [Cmd a] -> Path a
closedPath = Path True
openPath = Path False

_m, _M, _l, _L :: a -> a -> Cmd a
_m = MoveTo False
_M = MoveTo True
_l = LineTo False
_L = LineTo True

{-
_z :: Cmd a
_z = Close
-}

