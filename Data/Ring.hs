{-# LANGUAGE NoImplicitPrelude #-}
{- OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Ring where

import GHC.Base (Eq((==)), Ord((<=)))
import GHC.Real (Rational())
import Data.Functor (Functor(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative(..))
--import Data.Monoid (Sum(Sum, getSum), Product (Product, getProduct))
import Data.Group

type Num = Ring
class (Ring a, Ord a) => Real a where toRational :: a -> Rational
type Fractional a = DivisionRing a
type Integral a = EuclideanDomain a

newtype Sum a = Sum {getSum :: a} deriving (Eq, Functor, Foldable, Traversable)
instance Applicative Sum where
  pure = Sum
  Sum x <*> Sum y = Sum (x y)

newtype Product a = Product {getProduct :: a} deriving (Eq, Functor, Foldable, Traversable)

instance Applicative Product where
  pure = Product
  Product x <*> Product y = Product (x y)

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
