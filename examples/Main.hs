{-# LANGUAGE CPP #-}

module Main (main) where

import System.IO (IO, print, putStr, putStrLn)
import GHC.Base (Ordering(..))
import Data.Group
import Data.Ring

#define TEST(expr) putStr "expr\n> "; print (expr)

main :: IO ()
main = do
  putStrLn "Data.Group examples: (need better demo of Group class)"
  TEST ("hello, " <> "world!")
  TEST (EQ <> EQ <> GT <> EQ <> LT)
  TEST ("gree" <> mempty <> "tings")
  TEST (((), mempty) <> (mempty, ()))
  TEST (invert () <> ())
  putStrLn ""

  putStrLn "Data.Ring / Data.Numbers (reimplementations of existing number types) examples:"
  TEST ((5 :: Int) * 3 - 2)
  TEST (18 + (2 :: Rational) / 2 + 10)
  TEST ((1 + 2 + (3 :: Double)) * 100 / 25)
  putStrLn ""

  putStrLn "Data.Ring / Data.Numbers (new number types) examples:"
  TEST (Vec2 (12 :: Integer) 100 + Vec2 3 2 * (Vec2 18 7 `div` Vec2 6 6))
  TEST ((Vec3 0 (0 :: Float) 0 - Vec3 99 99 99) / Vec3 3 9 11)

