{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Numbers (
  Int,
  Word,
  Integer,
  Float,
  Double,
  Rational,
  Vec2(..),
  Vec3(..)
  ) where

import Data.Int (Int)
import Data.Word (Word)
import GHC.Integer (Integer)
import GHC.Float (Float, Double)
import Data.Ratio (Rational)
import qualified GHC.Num (Num(..))
import qualified GHC.Real (Fractional(..), Integral(..))

import Data.Tuple (fst, snd)
import Data.Functor (Functor(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), sequenceA)
import Control.Applicative (Applicative(..))
import Data.Function (($))
import GHC.Base (Eq)
import Text.Show (Show)

import Data.Group
import Data.Group.Wrappers
import Data.Ring

--

#define REWRITE_NUM(type) \
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

#define REWRITE_FRACTIONAL(type) \
  instance Group (Product type) where \
    { invert (Product x) = Product (GHC.Real.recip x) };\
  instance DivisionRing type where \
    { fromRational = GHC.Real.fromRational; \
      (/) = (GHC.Real./); \
      recip = GHC.Real.recip };

#define REWRITE_INTEGRAL(type) \
  instance CommutativeRing type; \
  instance EuclideanDomain type where \
    { quotRem = GHC.Real.quotRem; };

REWRITE_NUM(Int)
REWRITE_NUM(Word)
REWRITE_NUM(Integer)
REWRITE_NUM(Float)
REWRITE_NUM(Double)
REWRITE_NUM(Rational)
REWRITE_FRACTIONAL(Float)
REWRITE_FRACTIONAL(Double)
REWRITE_FRACTIONAL(Rational)
REWRITE_INTEGRAL(Int)
REWRITE_INTEGRAL(Word)
REWRITE_INTEGRAL(Integer)

--

#define FIXEDVEC_GROUPS(type, type2) \
  instance Semigroup (type2 a) => Semigroup (type2 (type a)) where \
    { v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2) }; \
  instance Monoid (type2 a) => Monoid (type2 (type a)) where \
    { mempty = sequenceA (pure mempty) }; \
  instance Group (type2 a) => Group (type2 (type a)) where \
    { invert v = sequenceA $ invert <$> sequenceA v }; \
  instance Abelian (type2 a) => Abelian (type2 (type a));

#define FIXEDVEC_NUMERIC(type) \
  instance Semiring a => Semiring (type a) where \
    { v1 + v2 = (+) <$> v1 <*> v2; \
      v1 * v2 = (*) <$> v1 <*> v2; \
      zero = pure zero; \
      one = pure one; }; \
  instance Ring a => Ring (type a) where \
    { v1 - v2 = (-) <$> v1 <*> v2; \
      negate = fmap negate; \
      abs = fmap abs; \
      signum = fmap abs; }; \
  instance DivisionRing a => DivisionRing (type a) where \
    { fromRational x = pure (fromRational x); \
      v1 / v2 = (/) <$> v1 <*> v2; \
      recip = fmap recip; }; \
  instance CommutativeRing a => CommutativeRing (type a); \
  instance EuclideanDomain a => EuclideanDomain (type a) where \
    { v0 `quotRem` v1 = unzip $ quotRem <$> v0 <*> v1 }; \
  instance Field a => Field (type a);

unzip :: Functor f => f (a,b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

class (Applicative v, Traversable v) => FixedVector v

data Vec2 a = Vec2 a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec2 where
  pure x = Vec2 x x
  (Vec2 a1 a2) <*> (Vec2 b1 b2) = Vec2 (a1 b1) (a2 b2)
instance FixedVector Vec2

FIXEDVEC_GROUPS(Vec2, Sum)
FIXEDVEC_GROUPS(Vec2, Product)
FIXEDVEC_NUMERIC(Vec2)


data Vec3 a = Vec3 a a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec3 where
  pure x = Vec3 x x x
  (Vec3 a1 a2 a3) <*> (Vec3 b1 b2 b3) = Vec3 (a1 b1) (a2 b2) (a3 b3)
instance FixedVector Vec3

FIXEDVEC_GROUPS(Vec3, Sum)
FIXEDVEC_GROUPS(Vec3, Product)
FIXEDVEC_NUMERIC(Vec3)
