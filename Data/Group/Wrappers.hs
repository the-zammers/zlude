{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Group.Wrappers 
    ( Sum (Sum, getSum)
    , Product (Product, getProduct)
    , Endo (Endo, appEndo)
    ) where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Functor (Functor)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (Read)
import Text.Show (Show)
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))
import Data.Function (id, (.))

import Data.Group

newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Functor, Foldable, Traversable, Read, Show)

instance Applicative Sum where
    pure = Sum
    Sum x <*> Sum y = Sum (x y)

instance Monad Sum where
    x >>= f = f (getSum x)

--

newtype Product a = Product {getProduct :: a} deriving (Eq, Ord, Functor, Foldable, Traversable, Read, Show)

instance Applicative Product where
    pure = Product
    Product x <*> Product y = Product (x y)

instance Monad Product where
    x >>= f = f (getProduct x)

--

newtype Endo a = Endo {appEndo :: a -> a}

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (g . f)

instance Monoid (Endo a) where
    mempty = Endo id

