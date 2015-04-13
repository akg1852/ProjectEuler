import Data.List (sort, nub, permutations, foldl1', maximumBy)
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.Ratio (numerator)
import Data.Functor

import Euler.Factor
import Euler.General
import Euler.Special

-- 1: Sum of all multiples of 3 and 5 below 1000
euler1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- 2: Sum of all even Fibonacci numbers under 4 million
euler2 = sum [x | x <- (takeWhile (<= 4000000) fib), even x]

-- 3: Largest prime factor a number
euler3 = last $ primeFactors 600851475143

-- 4: Largest palindromic product of two 3-digit numbers
euler4 = maximum [z | x <- [100..999], y <- [1..x], let z = x*y, (show z) == (reverse . show $ z)]

-- 5: Smallest multiple of 1-20
euler5 = foldl1 lcm [1..20]

-- 6: Sum square difference of 1-100
euler6 = ((^2) . sum $ xs) - (sum . map (^2) $ xs)
    where xs = [1..100]

-- 7: 10001st prime
euler7 = primes !! 10000

-- 8: Largest product of 13 digits in a series
euler8 = do
    file <- readFile "files/large-product.txt"
    let series = map digitToInt file
    return $ maxProdInSeries 13 series
  where
    maxProdInSeries n xs
        | (length xs < n) = 0
        | otherwise       = max (product . take n $ xs) (maxProdInSeries n . tail $ xs)

-- 9: Special Pythagorean triplet
euler9 = head [(a*b*c) * (1000 `div` (a+b+c))^3 |
    c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, 1000 `mod` (a+b+c) == 0]

-- 10: Summation of primes below 2 million
euler10 = sum . takeWhile (< 2000000) $ primes

-- 11: Largest product of 4 in a 20*20 grid
euler11 = do
    file <- readFile "files/grid.txt"
    let g = getLineElements file
    return $ maxProd4InGrid g 0 0
maxProd4InGrid g r c
    | c > cMax  = maxProd4InGrid g (r+1) (c-cMax-1)
    | r > rMax  = 0
    | otherwise = maximum [
        if (r `elem` [3..rMax] && c `elem` [0..(cMax-3)]) -- up right
            then product [g!!r!!c, g!!(r-1)!!(c+1), g!!(r-2)!!(c+2), g!!(r-3)!!(c+3)] else 0,
        if (r `elem` [0..rMax] && c `elem` [0..(cMax-3)]) -- right
            then product [g!!r!!c, g!!r!!(c+1), g!!r!!(c+2), g!!r!!(c+3)] else 0,
        if (r `elem` [0..(rMax-3)] && c `elem` [0..(cMax-3)]) -- down right
            then product [g!!r!!c, g!!(r+1)!!(c+1), g!!(r+2)!!(c+2), g!!(r+3)!!(c+3)] else 0,
        if (r `elem` [0..(rMax-3)] && c `elem` [0..cMax]) -- down
            then product [g!!r!!c, g!!(r+1)!!c, g!!(r+2)!!c, g!!(r+3)!!c] else 0,
        maxProd4InGrid g r (c+1) -- the rest
    ]
  where
    rMax = (length g) - 1
    cMax = length (g !! 0) - 1

-- 12: Highly divisible triangular number
euler12 = head . dropWhile ((<= 500) . numDivisors) $ map tri [2..]
    where tri n = n * (n+1) `div` 2

-- 13: Large Sum (first 10 digits)
euler13 = take 10 . digits . sum . map read . lines <$> readFile "files/large-sum.txt"

-- 14: Longest Collatz sequence
euler14 = foldl1' (\a b -> if snd a > snd b then a else b) (map (\x -> (x, collatz x)) [500000..999999])

-- 15: Lattice paths - number of routes through a grid
euler15 = paths 20 20
    where paths rows cols = (rows + cols) `choose` rows

-- 16: Power digit sum
euler16 = sum $ digits (2^1000)

-- 17: Number letter counts
euler17 = sum $ map numberNameLength [1..1000]

-- 18: see #67

-- 19: Counting Sundays (Number of Sundays on the first of the month during the twentieth century)
euler19 = length [1 | y <- [1901..2000], m <- [1..12], (dayOfTheWeek y m 1) == "Sunday"]

-- 20: Factorial digit sum
euler20 = sum . digits . factorial $ 100

-- 21: amicable numbers
euler21 = sum [a | a <- [2..9999], let b = sumProperDivisors a, a /= b, b /= 1, sumProperDivisors b == a]

-- 22: Name Scores
euler22 =  sum . zipWith (*) [1..] . map wordValue . sort . splitOn ',' <$> readFile "files/names.txt"

-- 23: Non-abundant sums
-- (slow: takes ~4.5 minutes to run)
euler23 = sum $ filter nonAbundantSum [1..28123]
  where
    nonAbundantSum n = null [x | let aa = takeWhile (<n) abundant, x <- aa, let y = n - x, y `elem` aa]
    abundant = filter (\n -> sumProperDivisors n > n) [2..]

-- 24: Lexicographic permutations
euler24 = undigits . (!! 999999) . sort . permutations $ [0..9]

-- 25: 1000-digit Fibonacci number
euler25 = length . takeWhile (< 10^999) $ fib

-- 27: Quadratic primes
euler27 = snd . maximumBy (comparing fst) $ [(length q, a * b) | a <- range, b <- range, let q = takeWhile isPrime $ map (formula a b) [1..]]
  where
    range = [-1000..1000]
    formula a b n = n^2 + a*n + b

-- 28: Sum of the diagonals in an ulam spiral
euler28 = sum . ulumDiags $ 1001

-- 29: Distinct powers of a^b for a and b in [2..100]
euler29 = length . nub $ [a^b | a <- [2..100], b <- [2..100]]

-- 30: Digit fifth powers
euler30 = sum $ filter go [10..999999]
    where go n = (== n) . sum . map (^5) $ digits n

-- 31: Number of ways of making £2
euler31 = length . coinCombos 200 $ [200, 100, 50, 20, 10, 5, 2, 1]
  where
    coinCombos total coins
        | null remCoins = [[]]
        | otherwise     = concat [map (x:) . coinCombos (total - x) . dropWhile (> x) $ remCoins | x <- remCoins]
        where remCoins = dropWhile (> total) coins

-- 37: Sum of all (doubly) truncatable primes
euler37 = sum . take 11 . filter (\p -> trunc tail p && trunc init p) $ drop 4 primes
  where
    trunc f p
        | p < 10 = True
        | otherwise = let n = read . f . show $ p in isPrime n && trunc f n

-- 42: Triangle words
euler42 = length . filter isTriangle . map wordValue . splitOn ',' <$> readFile "files/words.txt"
    where isTriangle n = isInteger ((sqrt (8 * fromIntegral n + 1) - 1) / 2)

-- 46: Smallest odd composite not the sum of a prime and twice a square
euler46 = head [ n | n <- [3,5..], not $ isPrime n,
    all (\r -> odd r || not (isSquare (r `div` 2))) $ map (n-) . takeWhile (<n) $ primes]

-- 48: Sum of first 1000 Self powers (last 10 digits)
euler48 = last10digits . sum $ [n^n | n <- [1..1000]]
    where last10digits = reverse . take 10 . reverse . show

-- 52: Permuted multiples
euler52 = head [n | n <- [1..], let m = map (*n) [1..6], anagrams m]
  where
    anagrams = allTheSame . map (sort . show)
    allTheSame xs = all (== head xs) (tail xs)

-- 53: Combinations of [1..100] greater than 1-million
euler53 = length [c | n <- [1..100], r <- [0..n], let c = n `choose` r, c > 1000000]

-- 65: Convergents of e
euler65 = sum . digits . numerator $ e 99
e n = (+2) . go n $ (\x -> [1,x,1]) =<< [2,4..]
    where go 0 _ = 0
          go n (x:xs) = 1 / (x + (go (n-1) xs))

-- 67: Maximum Path Sum
euler67 = maxPathSum . reverse . getLineElements <$> readFile "files/triangle.txt"

-- 89: Roman numerals
euler89 = do
    file <- readFile "files/roman.txt"
    let before = lines file
    let after = map (toRoman . fromRoman) before
    return $ (length $ concat before) - (length $ concat after)

-- 113: Non-bouncy numbers below 10^100
euler113 = sum $ map (\d -> ascending d + descending d - 9) [1..100]
  where
    ascending d = (d+8) `choose` d
    descending d = (d+9) `choose` d - 1
