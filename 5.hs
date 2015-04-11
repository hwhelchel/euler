--2520 is the smallest number that can be divided
--by each of the numbers from 1 to 10 without any remainder.

--What is the smallest positive number that is evenly divisible
--by all of the numbers from 1 to 20?

import Data.List

divisibleBy :: [Int] -> Int -> Bool
divisibleBy [] _ = False
divisibleBy xs num = and $ map (\x -> num `mod` x == 0) xs

smallestDivisibleBy :: [Int] -> [Int] -> Maybe Int
smallestDivisibleBy [] _ = Nothing
smallestDivisibleBy _ [] = Nothing
smallestDivisibleBy xs ys = find (divisibleBy xs) ys

-- more efficient and simple version
smallestDivisibleBy' :: [Int] -> Int
smallestDivisibleBy' = foldl (lcm) 1
