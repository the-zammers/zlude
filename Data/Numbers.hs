{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Data.Numbers (
  Int,
  Word,
  Integer,
  Float,
  Double,
  Rational
  ) where

import Data.Int (Int)
import Data.Word (Word)
import GHC.Integer (Integer)
import GHC.Float (Float, Double)
import Data.Ratio (Rational)
import qualified GHC.Num (Num(..))
import qualified GHC.Real (Fractional(..))

import Data.Group
import Data.Ring

--

#define REWRITENUM(type) \
   instance Semigroup (Sum type) where \
     { (Sum x) <> (Sum y) = Sum (x GHC.Num.+ y) };\
   instance Monoid (Sum type) where \
     { mempty = Sum 0 };\
   instance Group (Sum type) where \
     { invert (Sum x) = Sum (GHC.Num.negate x) };\
   instance Abelian (Sum type);\
   instance Semigroup (Product type) where \
     { (Product x) <> (Product y) = Product (x GHC.Num.* y) };\
   instance Monoid (Product type) where \
     { mempty = Product 1 };\
   instance Abelian (Product type); \
   instance Semiring type where \
     { (+) = (GHC.Num.+); \
       (*) = (GHC.Num.*); \
       zero = 0; \
       one = 1 };\
   instance Ring type where \
     { negate = GHC.Num.negate; \
       abs = GHC.Num.abs; \
       signum = GHC.Num.signum };

#define REWRITEFRACTIONAL(type) \
  instance Group (Product type) where \
    { invert (Product x) = Product (GHC.Real.recip x) };\
  instance DivisionRing type where \
    { fromRational = GHC.Real.fromRational; \
      (/) = (GHC.Real./); \
      recip = GHC.Real.recip };

REWRITENUM(Int)
REWRITENUM(Word)
REWRITENUM(Integer)
REWRITENUM(Float)
REWRITENUM(Double)
REWRITENUM(Rational)
REWRITEFRACTIONAL(Float)
REWRITEFRACTIONAL(Double)
REWRITEFRACTIONAL(Rational)

-- instance CommutativeRing Int where
