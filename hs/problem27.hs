module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the
consecutive values n = 0 to 39. However, when n = 40, 40² + 40 + 41 =
40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² +
41 + 41 is clearly divisible by 41.

The incredible formula n² − 79n + 1601 was discovered, which produces
80 primes for the consecutive values n = 0 to 79. The product of the
coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n e.g. |11| = 11 and |−4| =
4 Find the product of the coefficients, a and b, for the quadratic
expression that produces the maximum number of primes for consecutive
values of n, starting with n = 0.
-}

type Quad = (Int, Int)

eval :: Quad -> Int -> Int
eval (a, b) x = x^2 + a*x + b

isPrime :: Int -> Bool
isPrime n | n > 1     = and [n `mod` m /= 0
                              | m <- takeWhile ((<= n) . (^2)) (2:[3,5..])]
          | otherwise = False

primesList = map isPrime [0..]

isPrime' :: Int -> Bool
isPrime' n | n > 1     = primesList !! n
           | otherwise = False

bestQuad :: Quad
bestQuad = maximumBy (comparing numPrimes)
                     [(a, b) | b <- filter isPrime' [2..999],
                               a <- [(1-b)..999]]

numPrimes :: Quad -> Int
numPrimes q = length $ takeWhile isPrime' $ eval q <$> [0..]

main :: IO ()
main = print $ uncurry (*) bestQuad
