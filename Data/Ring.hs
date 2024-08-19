{-# LANGUAGE NoImplicitPrelude #-}

module Data.Ring where

import GHC.Base (Eq((==)), Ord)
import GHC.Real (Rational()) --relies on base because there's a loop otherwise
import Data.Group
import Data.Group.Wrappers

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
