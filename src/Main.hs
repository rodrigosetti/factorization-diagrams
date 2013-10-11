{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import Data.List (find)
import Data.Maybe (fromJust)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

-- | Return True if the first number divides the second
divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0

-- | The infinite sequence of prime numbers
primes :: [Integer]
primes = sieve [2..] where
             sieve [] = []
             sieve (x:xs) = x : filter (not . divides x) (sieve xs)

-- | Return the list of prime factors (with repetitions) for a positive integer
factor :: Integer -> [Integer]
factor n
    | n < 2 = []
    | otherwise = let d = fromJust $ find (`divides` n) primes in
                   d : factor (n `div` d)

-- | Given an integer, returns it's factorization diagram
factorDiagram :: (TrailLike (QDiagram b R2 m), Semigroup m) => Integer -> QDiagram b R2 m
factorDiagram =
    compose . map fromInteger . factor
  where
    compose [] = circle 1 # lw 0 # fc black
    compose (x:xs) = mconcat $ map ((`rotateBy` d') . Turn) [1/x, 2/x .. x/x] where
                      d' = if w > h then translateY r d else translateX r d
                      r = max w h * 0.5 / sin (tau / (2 * x))
                      d = compose xs
                      w = width d
                      h = height d


main :: IO ()
main = getLine >>= defaultMain . factorDiagram . read

