import Data.List

main = putStrLn $ show $ maxPalindromeProduct 3

maxPalindromeProduct digits = last $ sort allPalindromes
  where
    maxn = (10 ^ digits) - 1
    (s1, s2) = ([1 .. maxn], [1 .. maxn])
    allPalindromes = [x * y | x <- s1, y <- s2, isPalindrome (x * y)]

isPalindrome :: Int -> Bool
isPalindrome n = isPalindrome' (show n)

isPalindrome' :: String -> Bool
isPalindrome' n
  | length n <= 1 = True
  | otherwise = (head n) == (last n) && isPalindrome' (init $ tail n)
