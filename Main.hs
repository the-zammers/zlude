{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import System.IO (IO, print)
import Control.Applicative (pure)
import Data.Group
import Data.Ring
import Data.Numbers
import Data.NumbersTwo


main :: IO ()
main = do
  print ((5 :: Int) * 3 - 2)
  print (Vec2 (12 :: Float) 84 + Vec2 3 2 * Vec2 18 7)

