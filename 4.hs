--A palindromic number reads the same both ways.
--The largest palindrome made from the product of two 2-digit
--numbers is 9009 = 91 × 99.

--Find the largest palindrome made from the product of two 3-digit numbers.

import Data.Digits
import Data.List

isPalindromic :: [Int] -> Bool
isPalindromic [] = True
isPalindromic [x] = True
isPalindromic xs
  | head xs == last xs = isPalindromic $ tail (init xs)
  | otherwise = False

palindromes :: [Int] -> Int
palindromes = head . reverse . sort . filter (isPalindromic . digits 10)

largestPalindrome :: Int
largestPalindrome = palindromes [ x * y | x <- [100..999], y <- [100..999] ]
