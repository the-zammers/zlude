{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.NumbersTwo where

import Data.Group
import Data.Ring
import Data.Tuple (fst, snd)
import Data.Functor (Functor(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), sequenceA)
import Control.Applicative (Applicative(..))
import Data.Function (($))
import GHC.Base (Eq)
import Text.Show (Show)

unzip :: Functor f => f (a,b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

data Vec2 a = Vec2 a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec2 where
  pure x = Vec2 x x
  (Vec2 a1 a2) <*> (Vec2 b1 b2) = Vec2 (a1 b1) (a2 b2)

instance Semigroup (Sum a) => Semigroup (Sum (Vec2 a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance Monoid (Sum a) => Monoid (Sum (Vec2 a)) where
  mempty = sequenceA (pure mempty)
instance Group (Sum a) => Group (Sum (Vec2 a)) where
  invert v = sequenceA $ invert <$> sequenceA v
instance Abelian (Sum a) => Abelian (Sum (Vec2 a))
instance Semigroup (Product a) => Semigroup (Product (Vec2 a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance Monoid (Product a) => Monoid (Product (Vec2 a)) where
  mempty = sequenceA (pure mempty)
instance Group (Product a) => Group (Product (Vec2 a)) where
  invert v = sequenceA $ invert <$> sequenceA v
instance Abelian (Product a) => Abelian (Product (Vec2 a))
instance Semiring a => Semiring (Vec2 a) where
  v1 + v2 = (+) <$> v1 <*> v2
  v1 * v2 = (*) <$> v1 <*> v2
  zero = pure zero
  one = pure one
instance Ring a => Ring (Vec2 a) where
  v1 - v2 = (-) <$> v1 <*> v2
  negate = fmap negate
  abs = fmap abs
  signum = fmap abs
instance DivisionRing a => DivisionRing (Vec2 a) where
  fromRational x = pure (fromRational x)
  v1 / v2 = (/) <$> v1 <*> v2
  recip = fmap recip
instance CommutativeRing a => CommutativeRing (Vec2 a)
instance EuclideanDomain a => EuclideanDomain (Vec2 a) where
  v0 `quotRem` v1 = unzip $ quotRem <$> v0 <*> v1
instance Field a => Field (Vec2 a)


data Vec3 a = Vec3 a a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec3 where
  pure x = Vec3 x x x
  (Vec3 a1 a2 a3) <*> (Vec3 b1 b2 b3) = Vec3 (a1 b1) (a2 b2) (a3 b3)
instance Semigroup (Sum a) => Semigroup (Sum (Vec3 a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance Monoid (Sum a) => Monoid (Sum (Vec3 a)) where
  mempty = sequenceA (pure mempty)
instance Group (Sum a) => Group (Sum (Vec3 a)) where
  invert v = sequenceA $ invert <$> sequenceA v
instance Abelian (Sum a) => Abelian (Sum (Vec3 a))
instance Semigroup (Product a) => Semigroup (Product (Vec3 a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance Monoid (Product a) => Monoid (Product (Vec3 a)) where
  mempty = sequenceA (pure mempty)
instance Group (Product a) => Group (Product (Vec3 a)) where
  invert v = sequenceA $ invert <$> sequenceA v
instance Abelian (Product a) => Abelian (Product (Vec3 a))
instance Semiring a => Semiring (Vec3 a) where
  v1 + v2 = (+) <$> v1 <*> v2
  v1 * v2 = (*) <$> v1 <*> v2
  zero = pure zero
  one = pure one
instance Ring a => Ring (Vec3 a) where
  v1 - v2 = (-) <$> v1 <*> v2
  negate = fmap negate
  abs = fmap abs
  signum = fmap abs
instance DivisionRing a => DivisionRing (Vec3 a) where
  fromRational x = pure (fromRational x)
  v1 / v2 = (/) <$> v1 <*> v2
  recip = fmap recip
instance CommutativeRing a => CommutativeRing (Vec3 a)
instance EuclideanDomain a => EuclideanDomain (Vec3 a) where
  v0 `quotRem` v1 = unzip $ quotRem <$> v0 <*> v1
instance Field a => Field (Vec3 a)
