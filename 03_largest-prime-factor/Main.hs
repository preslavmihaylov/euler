import Data.List

main = putStrLn $ show $ largestPrimeFactor 600851475143

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = last $ sort $ [x | x <- [1 .. sqrtn], n `mod` x == 0, isPrime x]
  where
    sqrtn = round $ sqrt $ fromIntegral n

isPrime :: Int -> Bool
isPrime n = [x | x <- [1 .. n], n `mod` x == 0] == [1, n]
