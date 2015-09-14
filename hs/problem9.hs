module Main where

import Control.Monad (guard)

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

isPythag :: Int -> Int -> Int -> Bool
isPythag a b c = a^2 + b^2 == c^2

specials :: [[Int]]
specials = do a <- [1..500]
              b <- [2..500]
              guard $ a < b
              let c = 1000 - a - b
              guard $ isPythag a b c
              return [a, b, c]

main :: IO ()
main = print . product $ head specials
