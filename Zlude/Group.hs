module Zlude.Group (
    Semigroup((<>), sconcat, stimes),
    Abelian,
    Monoid(mempty, mappend, mconcat),
    Group(invert, (~~), pow)
  ) where

import GHC.Base (Eq((==)), Ord(compare, (<=), (<)), ($), (.), Ordering(LT, EQ, GT), Maybe(Just, Nothing), IO, liftA2, pure, errorWithoutStackTrace)
import qualified GHC.Num (Num(negate))
import GHC.Real (Integral(quot), even)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (foldr)

import GHC.Num ((-)) --stimes still uses base Integral
import Data.List (map, (++))
import Data.Void (Void)

import Zlude.Bool (otherwise)

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

--


instance Semigroup [a] where
  (<>) = (++)
  {-# INLINE (<>) #-}

  stimes n x
    | n < 0 = errorWithoutStackTrace "stimes: [], negative multiplier"
    | otherwise = rep n
    where rep 0 = []
          rep i = x ++ rep (i - 1)

instance Semigroup Void where
  a <> _ = a
  stimes _ a = a

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \x -> f x <> g x
  stimes n f e = stimes n (f e)

instance Semigroup () where
  _ <> _ = ()
  sconcat _ = ()
  stimes _ _ = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  (a,b) <> (a',b') = (a <> a', b <> b')
  stimes n (a,b) = (stimes n a, stimes n b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
  (a,b,c) <> (a',b',c') = (a <> a', b <> b', c <> c')
  stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
  (a,b,c,d) <> (a',b',c',d') = (a <> a', b <> b', c <> c', d <> d')
  stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a,b,c,d,e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a <> a', b <> b', c <> c', d <> d', e <> e')
  stimes n (a,b,c,d,e) = (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT

  stimes n x = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Ordering, negative multiplier"
    EQ -> EQ
    GT -> x

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b = b
  a <> Nothing = a
  Just a <> Just b = Just (a <> b)

  stimes _ Nothing = Nothing
  stimes n (Just a) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

instance Semigroup a => Semigroup (IO a) where
  (<>) = liftA2 (<>)

---

instance Monoid [a] where
  {-# INLINE mempty #-}
  mempty = []
  {-# INLINE mconcat #-}
  mconcat xss = [x | xs <- xss, x <- xs]

instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  {-# INLINE mconcat #-}
  mconcat = \fs x -> mconcat $ map (\f -> f x) fs

instance Monoid () where
  mempty = ()
  mconcat _ = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
  mempty = (mempty, mempty, mempty)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
  mempty = (mempty, mempty, mempty, mempty)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a,b,c,d,e) where
  mempty = (mempty, mempty, mempty, mempty, mempty)

instance Monoid Ordering where
  mempty = EQ

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance Monoid a => Monoid (IO a) where
  mempty = pure mempty

instance Group () where
  invert _ = ()
  pow _ _ = ()

---

instance Group b => Group (a -> b) where
  invert f = invert . f
  pow f n e = pow (f e) n

instance (Group a, Group b) => Group (a,b) where
  invert (a,b) = (invert a, invert b)
  pow (a,b) n = (pow a n, pow b n)

instance (Group a, Group b, Group c) => Group (a,b,c) where
  invert (a,b,c) = (invert a, invert b, invert c)
  pow (a,b,c) n = (pow a n, pow b n, pow c n)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
  invert (a,b,c,d) = (invert a, invert b, invert c, invert d)
  pow (a,b,c,d) n = (pow a n, pow b n, pow c n, pow d n)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a,b,c,d,e) where
  invert (a,b,c,d,e) = (invert a, invert b, invert c, invert d, invert e)
  pow (a,b,c,d,e) n = (pow a n, pow b n, pow c n, pow d n, pow e n)

instance Abelian ()
instance Abelian b => Abelian (a -> b)
instance (Abelian a, Abelian b) => Abelian (a,b)
instance (Abelian a, Abelian b, Abelian c) => Abelian (a,b,c)
instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a,b,c,d)
instance (Abelian a, Abelian b, Abelian c, Abelian d, Abelian e) => Abelian (a,b,c,d,e)

