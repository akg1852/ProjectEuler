module Euler.Special where

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
