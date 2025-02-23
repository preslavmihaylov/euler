-- solution:
-- split all nums in range into factors
-- count factor occurence, but per num
-- eg [2, 6] -> [2], [3, 2] -> [2, 3] (because the latter 2 already occurred)

import Data.Map qualified as Map

main = do
  let nums = [1 .. 20]
  let allFactors = map factorsWithOccurences nums
  let productOfPows acc (factor, occ) = acc * factor ^ occ
  let result = foldl productOfPows 1 $ Map.toList $ foldl1 (Map.unionWith max) $ allFactors
  print result

factorsWithOccurences :: Int -> Map.Map Int Int
factorsWithOccurences n = occs
  where
    fs = factorsOf n 2
    occs = Map.mapWithKey (\k v -> occurences fs k) $ Map.fromList $ zip fs (repeat 1)

occurences :: [Int] -> Int -> Int
occurences [] t = 0
occurences (n : ns) t = (if n == t then 1 else 0) + occurences ns t

factorsOf :: Int -> Int -> [Int]
factorsOf 1 f = []
factorsOf n f
  | n `mod` f == 0 = f : factorsOf (n `div` f) f
  | otherwise = factorsOf n (f + 1)
