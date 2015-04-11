--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?

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
