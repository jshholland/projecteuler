module Main where

{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

divides :: Int -> Int -> Bool
n `divides` m = m `mod` n == 0

primes :: [Int]
primes = sieve [2..]
  where sieve []     = []
        sieve (x:xs) = x : sieve (filter (not . divides x) xs)

main :: IO ()
main = print $ primes !! 10000 -- blah, indexing
