module Main where

import Data.List (find)
import Data.Maybe (fromJust)

divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0

primes :: [Integer]
primes = sieve [2..] where
             sieve [] = []
             sieve (x:xs) = x : (filter (not . divides x) $ sieve xs)

factor :: Integer -> [Integer]
factor n
    | n < 2 = []
    | otherwise = let d = fromJust $ find (`divides` n) primes
                      n' = n `div` d in
                   d : (factor n')

main :: IO ()
main = putStrLn "Hello World"

