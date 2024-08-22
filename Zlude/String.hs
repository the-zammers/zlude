module Zlude.String
    ( String
    , IsString (fromString)
    , splitScalar, lines, unlines
    , tokenizeScalar, words, unwords
    ) where

import Data.String (String, IsString (..))

import Data.List (break, dropWhile, foldr1, concatMap, (++))
import Data.Char (isSpace)
import Data.Eq ((==))

import Zlude.Bool (Bool)

splitScalar :: (a -> Bool) -> [a] -> [[a]]
splitScalar _ [] = []
splitScalar f s = let (~l, s') = break f s in l : (case s' of
                [] -> []
                _:s'' -> splitScalar f s'')

lines :: String -> [String]
lines = splitScalar (== '\n')

unlines :: [String] -> String
unlines = concatMap (++ "\n")

tokenizeScalar :: (a -> Bool) -> [a] -> [[a]]
tokenizeScalar f s = case dropWhile f s of
                     [] -> []
                     s' -> let (w, s'') = break f s' in w : tokenizeScalar f s''

words :: String -> [String]
words = tokenizeScalar isSpace

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws
