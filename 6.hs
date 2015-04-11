--The sum of the squares of the first ten natural numbers is,
--1^2 + 2^2 + ... + 10^2 = 385

--The square of the sum of the first ten natural numbers is,
--(1 + 2 + ... + 10)^2 = 55^2 = 3025

--Hence the difference between the sum of the squares of
--the first ten natural numbers and the square of the sum
--is 3025 − 385 = 2640.

--Find the difference between the sum of the squares of
--the first one hundred natural numbers and the square of the sum.

sumOfSquaresFrom :: [Int] -> Int
sumOfSquaresFrom [] = 0
sumOfSquaresFrom xs = sum $ map (^2) xs

squareOfSumFrom :: [Int] -> Int
squareOfSumFrom [] = 0
squareOfSumFrom xs = (^2) $ sum xs

squareDifference :: [Int] -> Int
squareDifference [] = 0
squareDifference xs = subtract (sumOfSquaresFrom xs) (squareOfSumFrom xs)
