main = putStrLn $ show $ multiples 1000 [3, 5]

multiples limit factors = sum [x | x <- [1 .. (limit - 1)], any (\f -> x `mod` f == 0) factors]
