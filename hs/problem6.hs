module Main where

import Data.List (foldl')

{-
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

sumSquare :: [Int] -> Int
sumSquare = sum . map (^ 2)

squareSum :: [Int] -> Int
squareSum = (^ 2) . sum

main :: IO ()
main = print $ squareSum [1..100] - sumSquare [1..100]
