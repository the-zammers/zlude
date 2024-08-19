{-# LANGUAGE NoImplicitPrelude #-}

module Data.Group (
    Semigroup((<>), sconcat, stimes),
    Abelian,
    Monoid(mempty, mappend, mconcat),
    Group(invert, (~~), pow)
  ) where

import GHC.Base (Eq((==)), Ord(compare, (<=)), otherwise, ($), Ordering(LT, EQ, GT), (.), errorWithoutStackTrace)
import qualified GHC.Num (Num(negate))
import GHC.Real (Integral(quot), even)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (foldr)

---

infixr 6 <>
class Semigroup a where
  {-# MINIMAL (<>) | sconcat #-}
  (<>) :: a -> a -> a
  a <> b = sconcat (a :| [b])
  
  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as
    where go b (c:cs) = b <> go c cs
          go b []     = b

  stimes :: Integral b => b -> a -> a
  stimes y0 x0
    | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (y `quot` 2) x
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (y `quot` 2) (x <> z)

class Semigroup a => Abelian a

class Semigroup a => Monoid a where
  {-# MINIMAL mempty | mconcat #-}

  mempty :: a
  mempty = mconcat []
  {-# INLINE mempty #-}

  mappend :: a -> a -> a
  mappend = (<>)
  {-# INLINE mappend #-}

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
  {-# INLINE mconcat #-}

infixl 7 ~~
class Monoid a => Group a where
  {-# MINIMAL invert | (~~) #-}

  invert :: a -> a
  invert x = mempty ~~ x
  
  (~~) :: a -> a -> a
  x ~~ y = x <> invert y
  
  pow :: Integral x => a -> x -> a
  pow x0 n0 = case compare n0 0 of
    LT -> invert . f x0 $ GHC.Num.negate n0
    EQ -> mempty
    GT -> f x0 n0
    where
      f x n
        | even n = f (x `mappend` x) (n `quot` 2)
        | n == 1 = x
        | otherwise = g (x `mappend` x) (n `quot` 2) x
      g x n c
        | even n = g (x `mappend` x) (n `quot` 2) c
        | n == 1 = x `mappend` c
        | otherwise = g (x `mappend` x) (n `quot` 2) (x `mappend` c)
