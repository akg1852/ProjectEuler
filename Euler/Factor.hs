module Euler.Factor where

import Data.List (group)
import qualified Data.Map as Map (empty, lookup, insert, insertWith, delete)

-- list of primes
-- generated with a basic Sieve of Eratosthenes
-- from http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Integer]
primes = sieve [2..] Map.empty
  where
    sieve []     table = []
    sieve (x:xs) table =
        case Map.lookup x table of
            Nothing    -> x : sieve xs (Map.insert (x*x) [x] table)
            Just facts -> sieve xs (foldl reinsert (Map.delete x table) facts)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

-- prime test
isPrime :: Integer -> Bool
isPrime n = (== n) . head $ dropWhile (<n) primes

-- prime factors
primeFactors :: Integer -> [Integer]
primeFactors = go primes where
    go a@(p:ps) n = let (d,m) = n `divMod` p in
        if m == 0 then
            if d == 1 then [n]
            else p:(go a d)
        else go ps n
        
-- prime factors v2
primeFactors' :: Integer -> [(Integer, Int)]
primeFactors' = map (\xs@(x:_) -> (x, length xs)) . group . primeFactors

-- list all factorisations
factorise :: Integral a => a -> [[a]]
factorise n = concat [map (x:) xs |
    x <- [n, n-1 .. intSqrt n],
    let (d,m) = n `divMod` x,
    m == 0,
    let xs = if d == 1 then [[]] else factorise d]
  where
    intSqrt = ceiling . sqrt . fromIntegral

-- number of divisors
numDivisors :: Integer -> Int
numDivisors n = foldl (\acc ps -> (*acc) . succ $ length ps) 1 . group $ primeFactors n

-- sum of divisors
sumDivisors :: Integer -> Integer
sumDivisors = product . map (\(p,a) -> sum [p^i | i <- [0..a]]) . primeFactors'
sumProperDivisors :: Integer -> Integer
sumProperDivisors n = sumDivisors n - n