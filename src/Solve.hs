{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Solve where

import Expr
import Spec



euclDist :: Floating a => (a,a) -> (a,a) -> a
euclDist (x1,y1) (x2,y2) =
  sqrt $ (x2 - x1) ** 2
       + (y2 - y1) ** 2

