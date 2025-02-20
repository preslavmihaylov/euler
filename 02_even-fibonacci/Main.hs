main = putStrLn $ show $ sumEvenFibs

sumEvenFibs :: Integer
sumEvenFibs = sum [x | x <- (fibsUntil (< 4000000)), x `mod` 2 == 0]

fibsUntil :: (Integer -> Bool) -> [Integer]
fibsUntil cond = takeWhile cond fibs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
