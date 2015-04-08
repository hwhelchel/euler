sumEvenFib :: Int
sumEvenFib = sum . takeWhile (<4000000) $ filter even fibonacci
  where fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
