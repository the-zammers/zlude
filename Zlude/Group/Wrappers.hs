{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zlude.Group.Wrappers 
    ( Sum (Sum, getSum)
    , Product (Product, getProduct)
    , Endo (Endo, appEndo)
    , Dual (Dual, getDual)
    , All (All, getAll)
    , Any (Any, getAny)
    , Alt (Alt, getAlt)
    ) where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Bool (Bool(True, False), (&&), (||))
import Data.Functor (Functor(fmap))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (Read)
import Text.Show (Show)
import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (Monad((>>=)))
import Data.Function (id, (.))
import Data.Coerce (coerce)

import Zlude.Group

newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Functor, Foldable, Traversable, Read, Show)

instance Applicative Sum where
    pure = Sum
    (<*>) = coerce

instance Monad Sum where
    x >>= f = f (getSum x)

newtype Product a = Product {getProduct :: a} deriving (Eq, Ord, Functor, Foldable, Traversable, Read, Show)

instance Applicative Product where
    pure = Product
    (<*>) = coerce

instance Monad Product where
    x >>= f = f (getProduct x)

--

newtype Endo a = Endo {appEndo :: a -> a}

instance Semigroup (Endo a) where
    (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))

instance Monoid (Endo a) where
    mempty = Endo id

newtype Dual a = Dual {getDual :: a} deriving (Eq, Ord, Read, Show)

instance Semigroup a => Semigroup (Dual a) where
    Dual a <> Dual b = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty

instance Group a => Group (Dual a) where
    invert (Dual a) = Dual (invert a)

instance Functor Dual where
    fmap = coerce

instance Applicative Dual where
    pure = Dual
    (<*>) = coerce

instance Monad Dual where
    x >>= f = f (getDual x)

--

newtype All = All {getAll :: Bool} deriving (Eq, Ord, Read, Show)

instance Semigroup All where
    (<>) = coerce (&&)

instance Monoid All where
    mempty = All True

newtype Any = Any {getAny :: Bool} deriving (Eq, Ord, Read, Show)

instance Semigroup Any where
    (<>) = coerce (||)

instance Monoid Any where
    mempty = Any False

--
-- f a m lineup?, deal with stimesMonoid
newtype Alt f a = Alt {getAlt :: f a} deriving (Eq, Ord, Read, Show)

instance Alternative f => Semigroup (Alt f a) where
    (<>) = coerce ((<|>) :: f a -> f a -> f a)

instance Alternative f => Monoid (Alt f a) where
    mempty = Alt empty
