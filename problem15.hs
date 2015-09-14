module Main where

{-
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


How many such routes are there through a 20×20 grid?
-}

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

routes :: Integer -> Integer
routes n = fact (2*n) `div` (2 * fact n)

main :: IO ()
main = print $ routes 20
