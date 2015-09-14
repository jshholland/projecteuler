module Main where

import Control.Applicative ((<$>), (<*>))

{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}


divides :: Int -> Int -> Bool
divides n = (== 0) . (`mod` n)

allFactors :: [Int] -> Int -> Bool
allFactors xs n = all (`divides` n) xs

main :: IO ()
main = print . head . filter (allFactors [1..19]) $ [20,40..]
