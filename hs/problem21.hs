module Main where

import Control.Arrow ((&&&))
import Data.List (nub)

{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}

d :: Int -> Int
d = sum . divisors

divisors :: Int -> [Int]
divisors n = filter (/= n) .  concat $
  [nub [m, n `div` m] | m <- takeWhile ((<= n) . (^2)) [1..],
                        n `mod` m == 0]

areAmicable :: Int -> Int -> Bool
areAmicable n m = d n == m && d m == n

amicables :: [Int]
amicables = head . filter (\x -> head x == x !! 2) $ map (iterate d) [1..]

main :: IO ()
main = print . sum . map fst . filter (uncurry areAmicable) $ fmap (id &&& d) [1..10000]
