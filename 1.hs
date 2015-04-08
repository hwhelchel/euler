firstSum :: [Int] -> Int
firstSum = sum . takeWhile (<1000) . filter mult3Or5
  where mult3Or5 = (\x -> or $ map (==0) [x `mod` 3, x `mod` 5])

