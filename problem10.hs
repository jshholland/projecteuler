module Main where

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

divides :: Int -> Int -> Bool
n `divides` m = m `rem` n == 0

primes :: [Int]
primes = 2 : [i | i <- [3..],
                  and [i `rem` p > 0 | p <- takeWhile ((<= i) . (^2)) primes]]

sumUpTo :: Int -> Int
sumUpTo n = sum . takeWhile (< n) $ primes

main :: IO ()
main = print $ sumUpTo 2000000
