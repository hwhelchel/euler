--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

--Find the sum of all the primes below two million.

import Data.List
import Data.Maybe (listToMaybe)

factors :: Int -> [Int]
factors number = unfoldr nextPrime (2, number)
  where nextPrime = \(start, number) -> listToMaybe [(x, (x, div number x)) | number > 1, x <- [start..intSqrt number] ++ [number], rem number x == 0]

isPrime :: Int -> Bool
isPrime n = n > 1 && factors n == [n]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

sumPrimesLessThan :: Int -> Int
sumPrimesLessThan x = sum $ takeWhile (<x) $ filter (isPrime) [1..]
