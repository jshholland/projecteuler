module Main where

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

factors :: Int -> [Int]
factors = go [] 2
  where go _  _ 1 = []
        go fs p n | n `mod` p == 0 = p : go fs p (n `div` p)
                  | otherwise      = go fs (succ p) n

main :: IO ()
main = print . maximum . factors $ 600851475143
