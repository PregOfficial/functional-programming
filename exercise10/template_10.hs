import Data.List
{- Exercise 10.1.2 -}

powerlistFR :: [a] -> [[a]]
powerlistFR = foldr (\a b -> map ([a]++) b ++ b) [[]]

powerlistFL :: [a] -> [[a]]
powerlistFL = foldl (\b a -> b ++ map (++[a]) b) [[]]

{- Exercise 10.1.3 -}

sortFold :: Ord a => [a] -> [a]
sortFold = foldr insertFun []

insertFun :: Ord a => a -> [a] -> [a]
insertFun x xs = takeWhile (<x) xs ++ [x] ++ dropWhile(<x) xs

{- TESTS -}

testPowerlist f = sort (f xs) == sort (subsequences xs)
  where
    xs = [1..5]

testPowerlistFL = testPowerlist powerlistFL
testPowerlistFR = testPowerlist powerlistFR

testSortFold = sort xs == sortFold xs
  where
    xs = [2,1,1,4,5,6,2,3,7]

testTenPOne = testPowerlistFL && testPowerlistFR && testSortFold

testAll = testTenPOne
