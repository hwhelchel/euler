--By listing the first six prime numbers:
--2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?

import Data.List
import Data.Maybe (listToMaybe)

largestPrimeFactor :: Int -> Int
largestPrimeFactor = head . reverse . sort . factors

factors :: Int -> [Int]
factors number = unfoldr nextPrime (2, number)
  where nextPrime = \(start, number) -> listToMaybe [(x, (x, div number x)) | number > 1, x <- [start..intSqrt number] ++ [number], rem number x == 0]

isPrime :: Int -> Bool
isPrime n = n > 1 && factors n == [n]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

-- filter isPrime [2..] !! 10000
