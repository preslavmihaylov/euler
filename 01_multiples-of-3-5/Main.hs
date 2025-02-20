main = putStrLn $ show $ sumMultiples 1000 [3, 5]

sumMultiples :: Int -> [Int] -> Int
sumMultiples limit factors = sum [x | x <- [1 .. (limit - 1)], any (\f -> x `mod` f == 0) factors]
