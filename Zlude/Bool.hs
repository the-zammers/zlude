module Zlude.Bool
    ( Bool (True, False)
    , (&&), (||), not, xor
    , otherwise
    , bool
    , guard, guarded
    , unless, when
    ) where

import Data.Bool (Bool (..), (&&), (||), not, otherwise, bool)
import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (guard, unless, when)

xor :: Bool -> Bool -> Bool
x `xor` y = (x || y) && (not (x && y))

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty
{-# INLINE guarded #-}
