module Main where

import Control.Applicative
import Data.List (nub, tails)
import Data.Set (Set)
import qualified Data.Set as S

{-
A perfect number is a number for which the sum of its proper divisors
is exactly equal to the number. For example, the sum of the proper
divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
is a perfect number.

A number n is called deficient if the sum of its proper divisors is
less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
smallest number that can be written as the sum of two abundant numbers
is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant
numbers. However, this upper limit cannot be reduced any further by
analysis even though it is known that the greatest number that cannot
be expressed as the sum of two abundant numbers is less than this
limit.

Find the sum of all the positive integers which cannot be written as
the sum of two abundant numbers.
-}

divisors :: Integer -> [Integer]
divisors n = filter (/= n) . concat $
  [nub [m, n `div` m] | m <- takeWhile ((<= n) . (^2)) [1..],
                        n `mod` m == 0]

isAbundant :: Integer -> Bool
isAbundant n = n < sum (divisors n)

abundants :: [Integer]
abundants = filter isAbundant [1..]

sumsOfAbundants :: [[Integer]]
sumsOfAbundants = zipWith ($) (fmap (+) abundants) <$> tails abundants

abundantSumsUpTo :: Integer -> Set Integer
abundantSumsUpTo n = S.fromList . concat . takeWhile (not . null) . fmap (takeWhile (<= n)) $ sumsOfAbundants

triangle :: Integer -> Integer
triangle n = (n * (n+1)) `div` 2

limit :: Integer
limit = 28123

main :: IO ()
main = print $ triangle limit - sum (abundantSumsUpTo limit)
