{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.NumbersTwo where

import Data.Group
import Data.Ring
import Data.Functor (Functor(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..), sequenceA)
import Control.Applicative (Applicative(..))
import Data.Function (($))
import GHC.Base (Eq)
import Text.Show (Show)

class (Applicative v, Traversable v) => FixedVector v

data Vec2 a = Vec2 a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec2 where
  pure x = Vec2 x x
  (Vec2 a1 a2) <*> (Vec2 b1 b2) = Vec2 (a1 b1) (a2 b2)
instance FixedVector Vec2

data Vec3 a = Vec3 a a a deriving (Eq, Show, Functor, Foldable, Traversable)
instance Applicative Vec3 where
  pure x = Vec3 x x x
  (Vec3 a1 a2 a3) <*> (Vec3 b1 b2 b3) = Vec3 (a1 b1) (a2 b2) (a3 b3)
instance FixedVector Vec3

instance (FixedVector v, Semigroup (Sum a)) => Semigroup (Sum (v a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance (FixedVector v, Monoid (Sum a)) => Monoid (Sum (v a)) where
  mempty = sequenceA (pure mempty)
instance (FixedVector v, Abelian (Sum a)) => Abelian (Sum (v a))
instance (FixedVector v, Semigroup (Product a)) => Semigroup (Product (v a)) where
  v1 <> v2 = sequenceA ((<>) <$> sequenceA v1 <*> sequenceA v2)
instance (FixedVector v, Monoid (Product a)) => Monoid (Product (v a)) where
  mempty = sequenceA (pure mempty)
instance (FixedVector v, Semiring a) => Semiring (v a) where
  v1 + v2 = (+) <$> v1 <*> v2
  v1 * v2 = (*) <$> v1 <*> v2
  zero = pure zero
  one = pure one
