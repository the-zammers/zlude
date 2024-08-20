{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Ring (
  Semiring(..),
  Ring(..), Num,
  subtract,
  Real(..),
  DivisionRing(..), Fractional,
  CommutativeRing,
  EuclideanDomain(..), Integral,
  Field,
  Int,
  Word,
  Integer,
  Float,
  Double,
  Rational,
  Vec2(..),
  Vec3(..)
  ) where

import GHC.Base (Eq((==)), Ord)

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
import Text.Show (Show)

import Data.Group
import Data.Group.Wrappers

--

type Num = Ring
class (Ring a, Ord a) => Real a where toRational :: a -> Rational
type Fractional a = DivisionRing a
type Integral a = EuclideanDomain a

infixl 7 *
infixl 6 +,-
class (Monoid (Sum a), Monoid (Product a), Abelian (Sum a)) => Semiring a where
  (+) :: a -> a -> a
  x + y = getSum (Sum x <> Sum y)
  zero :: a
  zero = getSum mempty

  (*) :: a -> a -> a
  x * y = getProduct (Product x <> Product y)
  one :: a
  one = getProduct mempty

class (Semiring a, Group (Sum a), Monoid (Product a), Abelian (Sum a)) => Ring a where
  {-# MINIMAL (negate | (-)), abs, signum #-}

  {-# INLINE negate #-}
  negate :: a -> a -- getSum . invert . Sum
  negate x = zero - x

  {-# INLINE (-) #-}
  (-) :: a -> a -> a
  x - y = x + negate y

  abs :: a -> a
  signum :: a -> a

{-# INLINE subtract #-}
subtract :: Ring a => a -> a -> a
subtract x y = y - x

class (Ring a, Group (Sum a), Group (Product a), Abelian (Sum a)) => DivisionRing a where
  {-# MINIMAL fromRational, (recip | (/)) #-}

  fromRational :: Rational -> a

  {-# INLINE recip #-}
  recip :: a -> a -- getProduct . invert . Product
  recip x = one / x

  {-# INLINE (/) #-}
  (/) :: a -> a -> a
  x / y = x * recip y

class (Ring a, Group (Sum a), Monoid (Product a), Abelian (Sum a), Abelian (Product a)) => CommutativeRing a

class (Eq a, CommutativeRing a) => EuclideanDomain a where
  quotRem :: a -> a -> (a, a)

  divMod :: a -> a -> (a, a)
  divMod n d = if signum r == negate (signum d) then (q-one, r+d) else qr
               where qr@(q,r) = quotRem n d

  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE div #-}
  {-# INLINE mod #-}
  quot, rem, div, mod :: a -> a -> a
  n `quot` d = q where (q,_) = quotRem n d
  n `rem` d  = r where (_,r) = quotRem n d
  n `div` d  = q where (q,_) = divMod n d
  n `mod` d  = r where (_,r) = divMod n d

class (DivisionRing a, CommutativeRing a) => Field a

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
