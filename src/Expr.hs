{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Expr where

import Data.String (IsString(..))
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Real

type Env = Map String

data Expr
  = Val    Double
  | Var    String
  | Add    Expr Expr
  | Mul    Expr Expr
  | Neg    Expr
  | Recip  Expr
  | Abs    Expr
  | Signum Expr
  | Exp    Expr
  | Log    Expr
  | Sqrt   Expr
  | Pow    Expr Expr
  | Sin    Expr
  | Cos    Expr
  | Asin   Expr
  | Acos   Expr
  | Atan   Expr
  | Sinh   Expr
  | Cosh   Expr
  | Asinh  Expr
  | Acosh  Expr
  | Atanh  Expr
  | Min Expr Expr
  | Max Expr Expr
  deriving (Eq,Show)

ordExpr :: Expr -> [Int]
ordExpr = \case
  Val{}     -> [0]
  Var{}     -> [1]
  Add e1 e2 -> 2 : ordExpr e1 ++ ordExpr e2
  Mul e1 e2 -> 3 : ordExpr e1 ++ ordExpr e2
  Neg e     -> 4 : ordExpr e
  Recip e   -> 5 : ordExpr e
  Abs e     -> 6 : ordExpr e
  Signum e  -> 7 : ordExpr e
  Exp e     -> 8 : ordExpr e
  Log e     -> 9 : ordExpr e
  Sqrt e    -> 10 : ordExpr e
  Pow e1 e2 -> 11 : ordExpr e1 ++ ordExpr e2
  Sin e     -> 12 : ordExpr e
  Cos e     -> 13 : ordExpr e
  Asin e    -> 14 : ordExpr e
  Acos e    -> 15 : ordExpr e
  Atan e    -> 16 : ordExpr e
  Sinh e    -> 17 : ordExpr e
  Cosh e    -> 18 : ordExpr e
  Asinh e   -> 19 : ordExpr e
  Acosh e   -> 20 : ordExpr e
  Atanh e   -> 21 : ordExpr e
  Min e1 e2 -> 22 : ordExpr e1 ++ ordExpr e2
  Max e1 e2 -> 23 : ordExpr e1 ++ ordExpr e2

instance Ord Expr where
  compare e1 e2 = compare (ordExpr e1) (ordExpr e2)
  min = Min
  max = Max

ppExpr :: Expr -> String
ppExpr e = go 0 e ""
  where
  go :: Int -> Expr -> ShowS
  go d = \case
    Val v ->
      showsRationalPrec d $ toRational v
    Var x ->
      showString x
    Add e1 e2 ->
      showParen (d > 6)
      $ go 6 e1
      . showString " + "
      . go 7 e2
    Mul e1 e2 ->
      showParen (d > 7)
      $ go 7 e1
      . showString " * "
      . go 8 e2
    Neg e ->
      showParen (d > 6)
      $ showString "- "
      . go 7 e
    Recip e ->
      showParen (d > 7)
      $ showString "1 / "
      . go 8 e
    Abs e ->
      showParen (d > 10)
      $ showString "abs "
      . go 11 e
    Signum e ->
      showParen (d > 10)
      $ showString "signum "
      . go 11 e
    Exp e ->
      showParen (d > 10)
      $ showString "exp "
      . go 11 e
    Log e ->
      showParen (d > 10)
      $ showString "log "
      . go 11 e
    Sqrt e ->
      showParen (d > 10)
      $ showString "sqrt "
      . go 11 e
    Pow e1 e2 ->
      showParen (d > 8)
      $ go 9 e1
      . showString " ** "
      . go 8 e2
    Sin e ->
      showParen (d > 10)
      $ showString "sin "
      . go 11 e
    Cos e ->
      showParen (d > 10)
      $ showString "cos "
      . go 11 e
    Asin e ->
      showParen (d > 10)
      $ showString "asin "
      . go 11 e
    Acos e ->
      showParen (d > 10)
      $ showString "acos "
      . go 11 e
    Atan e ->
      showParen (d > 10)
      $ showString "atan "
      . go 11 e
    Sinh e ->
      showParen (d > 10)
      $ showString "sinh "
      . go 11 e
    Cosh e ->
      showParen (d > 10)
      $ showString "cosh "
      . go 11 e
    Asinh e ->
      showParen (d > 10)
      $ showString "asinh "
      . go 11 e
    Acosh e ->
      showParen (d > 10)
      $ showString "acosh "
      . go 11 e
    Atanh e ->
      showParen (d > 10)
      $ showString "atanh "
      . go 11 e
    Min e1 e2 ->
      showParen (d > 10)
      $ showString "min "
      . go 11 e1
      . showChar ' '
      . go 11 e2 
    Max e1 e2 ->
      showParen (d > 10)
      $ showString "max "
      . go 11 e1
      . showChar ' '
      . go 11 e2 

showsRationalPrec :: Int -> Rational -> ShowS
showsRationalPrec d r
  | den == 1
  = showsPrec d num
  | otherwise
  = showParen (d > 7)
    $ showsPrec 7 num
    . showString " / "
    . showsPrec 8 den
  where
  num = numerator r
  den = denominator r

-- instances {{{

instance Num Expr where
  (+)         = valFn2 (+) Add
  (*)         = valFn2 (*) Mul
  negate      = valFn1 negate Neg
  abs         = valFn1 abs Abs
  signum      = valFn1 signum Signum
  fromInteger = Val . fromInteger

instance Fractional Expr where
  recip        = valFn1 recip Recip
  fromRational = Val . fromRational

instance Floating Expr where
  pi    = Val pi
  exp   = valFn1 exp   Exp
  log   = valFn1 log   Log
  sqrt  = valFn1 sqrt  Sqrt
  (**)  = valFn2 (**)  Pow
  sin   = valFn1 sin   Sin
  cos   = valFn1 cos   Cos
  asin  = valFn1 asin  Asin
  acos  = valFn1 acos  Acos
  atan  = valFn1 atan  Atan
  sinh  = valFn1 sinh  Sinh
  cosh  = valFn1 cosh  Cosh
  asinh = valFn1 asinh Asinh
  acosh = valFn1 acosh Acosh
  atanh = valFn1 atanh Atanh

instance Real Expr where
  toRational = undefined

{-
    Val v -> toRational v
    Var x -> _
    Add e1 e2 -> toRational e1 + toRational e2
    Mul e1 e2 -> toRational e1 * toRational e2
    Neg e -> negate $ toRational e
    Recip e -> recip $ toRational e
    Abs e -> abs $ toRational e
    Signum e -> signum $ toRational e
    Exp e -> exp $ toRational e
    Log e -> log $ toRational e
    Sqrt e -> sqrt $ toRational e
    Pow e1 e2 -> toRational e1 ** toRational e2
    Sin e -> sin $ toRational e
    Cos e -> cos $ toRational e
    Asin e -> asin $ toRational e
    Acos e -> acos $ toRational e
    Atan e -> atan $ toRational e
    Sinh e -> sinh $ toRational e
    Cosh e -> cosh $ toRational e
    Asinh e -> asinh $ toRational e
    Acosh e -> acosh $ toRational e
    Atanh e -> atanh $ toRational e
-}

instance RealFrac Expr where
  properFraction = undefined

instance RealFloat Expr where
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined
  isNaN = undefined
  isInfinite = undefined
  isDenormalized = undefined
  isNegativeZero = undefined
  isIEEE = undefined

instance IsString Expr where
  fromString = Var

valFn1 :: (Double -> Double)
       -> (Expr -> Expr)
       -> Expr -> Expr
valFn1 fv fe = \case
  Val a -> Val $ fv a
  a     -> fe a

valFn2 :: (Double -> Double -> Double)
       -> (Expr -> Expr -> Expr)
       -> Expr -> Expr -> Expr
valFn2 fv fe = \case
  Val a -> \case
    Val b -> Val $ fv a b
    b     -> fe (Val a) b
  a     -> fe a

-- }}}

-- evaluation {{{

data EvalErr
  = UnboundVar String
  deriving (Eq,Ord,Show)

eval :: (Floating a, Ord a) => Env a -> Expr -> Either EvalErr a
eval env = evalWith (evalVar env) env

subst :: Env Expr -> Expr -> Expr
subst env e = case evalWith (return . Var) env e of
  Right e' -> e'
  Left err -> error $ "Unexpected error in subst: " ++ show err

evalVar :: Env a -> String -> Either EvalErr a
evalVar env x =
  maybe (Left $ UnboundVar x) return
  $ Map.lookup x env

evalWith :: (Floating a, Ord a) => (String -> Either EvalErr a) -> Env a -> Expr -> Either EvalErr a
evalWith unb env = go
  where
  go = \case
    Val v ->
      return $ realToFrac v
    Var x ->
      maybe (unb x) return
      $ Map.lookup x env
    Add e1 e2 ->
      (+) <$> go e1 <*> go e2
    Mul e1 e2 ->
      (*) <$> go e1 <*> go e2
    Neg e ->
      negate <$> go e
    Recip e ->
      recip <$> go e
    Abs e ->
      abs <$> go e
    Signum e ->
      signum <$> go e
    Exp e ->
      exp <$> go e
    Log e ->
      log <$> go e
    Sqrt e ->
      sqrt <$> go e
    Pow e1 e2 ->
      (**) <$> go e1 <*> go e2
    Sin e ->
      sin <$> go e
    Cos e ->
      cos <$> go e
    Asin e ->
      asin <$> go e
    Acos e ->
      acos <$> go e
    Atan e ->
      atan <$> go e
    Sinh e ->
      sinh <$> go e
    Cosh e ->
      cosh <$> go e
    Asinh e ->
      asinh <$> go e
    Acosh e ->
      acosh <$> go e
    Atanh e ->
      atanh <$> go e
    Min e1 e2 ->
      min <$> go e1 <*> go e2
    Max e1 e2 ->
      max <$> go e1 <*> go e2

-- }}}

