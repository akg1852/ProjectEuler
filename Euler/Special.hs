module Euler.Special where

import Data.Char (toUpper)
import Data.List (isPrefixOf)

-- length of collatz sequence starting on n
collatz :: Integral a => a -> a
collatz 1 = 1
collatz n
    | even n    = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (3*n + 1)

-- length of the written name of a number (excluding spaces and punctuation)
numberNameLength :: Num a => Int -> a
numberNameLength n
    | n <      20 = u !! n
    | n <     100 = let (d,m) = n `divMod`   10 in (t !! d) + numberNameLength m
    | n <    1000 = let (d,m) = n `divMod`  100 in (u !! d) + numberNameLength m + (if m > 0 then 10 else 7)
    | n < 1000000 = let (d,m) = n `divMod` 1000 in numberNameLength d + numberNameLength m  + (if m > 0 && m < 100 then 11 else 8)
    where u = [0,3,3,5,4,4,3,5,5,4,3,6,6,8,8,7,7,9,8,8]
          t = [0,0,6,6,5,5,5,7,6,6]

-- maximum path sum
-- input is assumed to be a triangle, ie of the form [[a], [b,c], [d,e,f], [g,h,i,j], ...]
-- output is the maximum total from top to bottom
maxPathSum :: Integral a => [[a]] -> a
maxPathSum [[x]] = x
maxPathSum (z:y:xs) = maxPathSum ((zipWith (+) (reduceRow z) y):xs)
    where reduceRow [x] = []
          reduceRow (z:y:xs) = (max z y):(reduceRow (y:xs))

-- day of the week for a given date
dayOfTheWeek :: Int -> Int -> Int -> String
dayOfTheWeek year month day = days !! (daysSince1900 `mod` 7)
    where days = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
          daysSince1900
              | (year - 1900) >= 0 = sum $ day : [daysInTheMonth year m | m <- [1..(month-1)]] ++
                                         [daysInTheMonth y m | y <- [1900..(year-1)], m <- [1..12]]
              | otherwise = negate . sum $ ((daysInTheMonth year month) - day) : [daysInTheMonth year m | m <- [(month+1)..12]] ++
                                         [daysInTheMonth y m | y <- [(year+1)..1899], m <- [1..12]]
          daysInTheMonth y m
              | m == 2                 = if leapYear then 29 else 28
              | m `elem` [9, 4, 6, 11] = 30
              | otherwise              = 31
              where leapYear = (y `mod` 4 == 0) && ((y `mod` 100 > 0) || (y `mod` 400 == 0))

-- diagonals of an ulum spiral
ulumDiags :: Integral a => a -> [a]
ulumDiags 0 = [0]
ulumDiags 1 = [1]
ulumDiags n = map ((+ head prev) . (*(n-1))) [4,3,2,1] ++ prev
    where prev = ulumDiags (n-2)

-- roman numerals
toRoman :: Integer -> String
toRoman i = go i romanNumerals
  where
    go 0 _ = ""
    go i numerals = let numerals' = dropWhile (\num -> snd num > i) numerals; num = head numerals'
        in (fst num) ++ (go (i - (snd num)) numerals')
fromRoman :: String -> Integer
fromRoman xs = go (map toUpper xs) romanNumerals
  where
    go "" _ = 0
    go xs numerals = case dropWhile (\num -> not (fst num `isPrefixOf` xs)) numerals of
        [] -> error "not a valid roman numeral"
        ns@(n:_) -> snd n + go (drop (length (fst n)) xs) (ns)
romanNumerals :: [(String, Integer)]
romanNumerals = [("M", 1000),
    ("CM", 900), ("D", 500), ("CD", 400), ("C", 100),
    ("XC", 90), ("L", 50), ("XL", 40), ("X", 10),
    ("IX", 9), ("V", 5), ("IV", 4), ("I", 1)]
