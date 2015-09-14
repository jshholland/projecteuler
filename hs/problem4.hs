module Main where

import Control.Monad (guard)

{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

isPalindrome :: Int -> Bool
isPalindrome n = reverse (show n) == show n

products :: [Int]
products = do x <- [100..999]
              y <- [100..999]
              let product = x*y
              guard $ isPalindrome product
              return product

main :: IO ()
main = print . maximum $ products
