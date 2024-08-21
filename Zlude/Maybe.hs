module Zlude.Maybe
    ( Maybe (Just, Nothing)
    , maybe, fromMaybe, (?:)
    , isJust, isNothing
    , maybeToAlternative, foldableToMaybe
    , catMaybes
    , mapMaybe
    ) where

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Function (id, flip, const, (.))
import Data.Foldable (Foldable (..))
import Control.Applicative (Applicative (..), Alternative (..))

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

maybeToAlternative :: Alternative f => Maybe a -> f a
maybeToAlternative = maybe empty pure

foldableToMaybe :: Foldable f => f a -> Maybe a
foldableToMaybe = foldr (const . Just) Nothing

catMaybes :: Foldable f => f (Maybe a) -> [a]
catMaybes = mapMaybe id

mapMaybe :: Foldable f => (a -> Maybe b) -> f a -> [b]
mapMaybe f = foldr (\x rest -> maybe rest (:rest) (f x)) []
