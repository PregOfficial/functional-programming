import Data.Char (ord, toUpper)

import Prelude hiding (zipWith)
-- since we have our own definition of zipWith in this file the previous
-- line hides the definition of zipWith from Prelude to avoid nameclashes

{- Exercise 11.1 -}

index 0 (x:xs) = x
index n (x:xs) = index (n-1) xs

zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

ones = 1 : ones
nats = 0 : zipWith (+) nats ones

get n = index n nats

{- Exercise 11.2 -}

greatest :: [Integer] -> Integer
greatest [] = error "empty list"
greatest (x : xs) = 
  if length xs == 0 then x 
  else if x > head xs then greatest (x : tail xs)
  else greatest xs

average :: [Double] -> Double
average [] = error "empty list"
average xs = average_aux 0 xs / fromIntegral (length xs)
  where 
    average_aux acc (x:xs) =
      if length xs == 0 then acc + x
      else average_aux (acc + x) xs


to_upper_str :: String -> String
to_upper_str = to_upper_str_aux ""
  where 
    to_upper_str_aux acc xs =
      if length xs == 0 then acc
      else to_upper_str_aux (acc ++ [toUpper (head xs)]) (tail xs) 

{- Exercise 11.3 -}


f :: Fractional t => t -> [t] -> t
f a [] = a
f a (x:xs) = g (a + x/3) xs

g :: Fractional t => t -> [t] -> t
g b [] = b
g b (x:xs) = f (b + x*x) (x:xs)

-- rewrite f and g to f' and g', using neither f, nor g, nor any form of mutual recursion
f' :: Fractional t => t -> [t] -> t
f' a xs = auxFG a xs 0

g' :: Fractional t => t -> [t] -> t
g' a xs = auxFG a xs 1 

auxFG :: Fractional t => t -> [t] -> Int -> t
auxFG a [] _ = a
auxFG a (x:xs) count = 
  if count `mod` 2 == 0
    then auxFG (a+x/3) xs (count+1)
  else
    auxFG (a + x*x) (x:xs) (count+1)

c :: String -> Int
c [] = 1
c xs = sum . map ord $ d xs

d :: String -> String
d [] = []
d (x:xs) = replicate (c xs) x ++ xs

-- rewrite c and d to c' and d', using neither c, nor d, nor any form of mutual recursion
c' :: String -> Int
c' [] = 1
c' (x:xs) = sum . map ord $ replicate (c' xs) x ++ xs

d' :: String -> String
d' [] = []
d' [a] = [a]
d' (x:xs) = replicate (sum (map ord (d' xs))) x ++ xs

{- Exercise 11.4 -}

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = x : xs
merge [] (y:ys) = y : ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | x == y = x : merge xs ys
  | x > y = y : merge (x:xs) ys
  | otherwise = []

fp_nums :: [Integer]
fp_nums = 1 : (
  merge (map (*5) fp_nums) 
    $ merge (map (*2) fp_nums) 
      (map (*3) fp_nums))

{- TESTS -}

testGreatest = and $ map f [[32,53,543,123,123], [23], [-12,90], [0,-1,-2]]
  where f x = maximum x == greatest x

testAverage = and $ map f [[32.3,53,543,123,123], [23], [-12,90], [0,-1.2,-2]]
  where f x = a x == average x
        a x = sum x / fromIntegral (length x)

testUpper = and $ map f ["Innsbruck", "Axams", "muenCHEN", []]
  where f x = map toUpper x == to_upper_str x

testF = and $ map g [(1, [3,23]), (321, [23,31])]
  where g (x,y) = f x y == f' x y

testG = and $ map f [(1, [3,23]), (321, [23,31])]
  where f (x,y) = g x y == g' x y

testC = and $ map f ["Inn", "Axa", "mue", []]
  where f x = c x == c' x

testD = and $ map f ["Inn", "Axa", "mue", []]
  where f x = d x == d' x

testMerge = and $ map f [([1,3], [2,4], [1,2,3,4])
                        ,([1,18,200], [19,150,200,300], [1,18,19,150,200,300])
                        ,([], [300], [300])
                        ,([300], [], [300])]
  where f (x,y,z) = merge x y == z

testNums = take 7 fp_nums == [1,2,3,4,5,6,8] && fp_nums !! 19 == 36

testAll = and [testGreatest,testAverage,testUpper,testF,testG,testC
               ,testD,testMerge,testNums]