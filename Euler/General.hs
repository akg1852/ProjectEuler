module Euler.General where

import Data.Char (ord, toUpper, digitToInt, intToDigit)
import qualified Data.Text as Text (pack, unpack, split)
import Data.Functor
import Control.Monad

-- check if integer is a perfect square
isSquare :: Integral a => a -> Bool
isSquare n = (== n) . (^2) . floor . sqrt $ fromIntegral n

-- check if floating point number is an integer
isInteger :: RealFrac a => a -> Bool
isInteger n = n == fromIntegral (round n)

-- fibonacci sequence
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

-- combinations
choose :: Integral a => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- permutations
permute :: Integral a => a -> a -> a
permute n 0 = 1
permute 0 k = 0
permute n k = permute (n-1) (k-1) * n

-- list the digits of a number
digits :: Integral a => a -> [Integer]
digits = map (toInteger . digitToInt) . show . fromIntegral

-- convert list of digits to integer
undigits :: Integral a => [a] -> Integer
undigits = read . map (intToDigit . fromIntegral)

-- factorial
factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- alpha value (A=1, B=2, etc)
alphaValue :: Char -> Int
alphaValue = subtract 64 . ord . toUpper
wordValue :: String -> Int
wordValue = sum . map alphaValue

-- string split
splitOn :: Char -> String -> [String]
splitOn c = map Text.unpack . Text.split (== c) . Text.pack

-- get Lines
getLineElements :: Read a => String -> [[a]]
getLineElements = map (map read . words) . lines