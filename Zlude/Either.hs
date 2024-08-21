module Zlude.Either
    ( Either (Left, Right)
    , either, swapEither
    , lefts, rights
    , isLeft, isRight
    , fromLeft, fromRight
    , partitionEithers
    , maybeToLeft, maybeFromLeft
    , maybeToRight, maybeFromRight
    ) where

import Data.Either (Either (..), either, isLeft, isRight, fromLeft, fromRight)
import Data.Foldable (Foldable(..))
import Data.Function (const)

import Zlude.Maybe

swapEither :: Either a b -> Either b a
swapEither = either Right Left

lefts :: Foldable f => f (Either a b) -> [a]
lefts = foldr (\x xs -> either (:xs) (const xs) x) []

rights :: Foldable f => f (Either a b) -> [b]
rights = foldr (\x xs -> either (const xs) (:xs) x) []

partitionEithers :: Foldable f => f (Either l r) -> ([l], [r])
partitionEithers = foldr go ([], [])
    where go x (ls,rs) = case x of
            Left l -> (l:ls, rs)
            Right r -> (ls, r:rs)

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight r = maybe (Left r) Right

maybeFromLeft :: Either l r -> Maybe l
maybeFromLeft = either Just (const Nothing)

maybeFromRight :: Either l r -> Maybe r
maybeFromRight = either (const Nothing) Just
