import Data.List

-- Exercise 9.1, Part 1
webData :: [String]
webData = ["Youtube", "Google", "Facebook", 
           "Youtube", "Facebook", "Youtube", 
           "Facebook", "Google", "Youtube"]

{- 9.1.1 Write a function that returns a list with unique 
  websites occuring in a dataset like webData -}
uniqueWebsites :: [String] -> [String]
uniqueWebsites [] = []
uniqueWebsites (x : xs) = x : uniqueWebsites (filter (x /=) xs)

{- 9.1.2 -}

count :: String -> [String] -> Int
count website dataSet = length (filter (website==) dataSet)

{- 9.1.3 Write a function that returns the ranking of 
  website visits using the count and uniqueWebsites 
  functions -}
result :: [String] -> [(Int, String)]
result dataSet = reverse (sort (map (\i -> (count i dataSet, i)) (uniqueWebsites dataSet)))

-- Exercise 9.1, Part 2 
webDataDuration :: [(String, Double)]
webDataDuration = [("Youtube", 2.5), ("Google", 23.2), 
                   ("Facebook", 23.2), ("Youtube", 3.4),
                   ("Facebook", 4.5), ("Youtube", 34.5), 
                   ("Facebook", 33.2),("Google", 34.3), 
                   ("Youtube", 12.4)]

{- 9.1.4 Write a new function to retrieve a list with
  unique websites occuring in a dataset like 
  webDataDuration -}
uniqueWebsites2 :: Fractional a => [(String, a)] -> [String]
uniqueWebsites2 list = uniqueWebsites (map fst list)

{- 9.1.5 Write a new function that returns the total time 
  website has been visited based on a dataset like 
  webDataDuration -}
count2 :: Fractional a => String -> [(String, a)] -> a
count2 website dataSet = sum (map (snd) (filter (\x -> website==fst x) dataSet))

{- 9.1.6 Write a higher-order function, similar to result,
  such that it can be used for the webData and 
  webDataDuration dataset by turning it into a 
  higher-order function -}

result2 :: (Ord a, Ord b) => (b -> c -> a) -> (c -> [b]) -> c -> [(a, b)]
result2 countFun uniqueFun = result2' where
    result2' dataSet = reverse (sort (map (\i -> (countFun i dataSet, i)) (uniqueFun dataSet)))


--- Exercise 9 Part 2: Rewrite function with foldr
{- 9.2.1-}

all_fold :: (a -> Bool) -> [a] -> Bool
all_fold _ [] = True
all_fold p xs = foldr (&&) True (map p xs) 

{- 9.2.2-}

dig2int :: [Integer] -> Integer
dig2int [] = 0
dig2int (x:xs) = x + 10 * dig2int xs

dig2int_fold :: [Integer] -> Integer
dig2int_fold [] = 0
dig2int_fold xs = foldr (\x y -> x + y * 10) 0 xs


--- Exercise 9 Part 3:

{-9.3.1-}

number :: [a] -> [(Int,a)]
number xs = zip [0..(length xs)] xs

{-9.3.2-}

evenProdSum :: [Int] -> Int
evenProdSum xs = sum [calcTuple x | x <- zip [1..(length xs)] xs, even (fst x)]

calcTuple :: (Int, Int) -> Int
calcTuple (a, b) = a * b


{- TESTS -}
testFunEq :: (Show a,Show b,Eq c) => (b -> c) -> (a -> b) -> [(a,b)] -> Bool
testFunEq eq f = all (\p -> eq (f (fst p)) == eq (snd p) || 
  error ("on input "++(show (fst p))++" output is "++(show (f (fst p)))++" but should be "++(show (snd p)))) 
testFun :: (Show a,Show b,Eq b) => (a -> b) -> [(a,b)] -> Bool
testFun = testFunEq id 
testFunFract f = all (\p -> abs ((f (fst p)) - snd p) < 1e-6 || 
  error ("on input "++(show (fst p))++" output is "++(show (f (fst p)))++" but should be "++(show (snd p))))
testFun2 testFun f l = testFun (\p -> f (fst p) (snd p)) (map (\(x,y,z) -> ((x,y),z)) l)

splitter [] = []
splitter xs = case span (\ (a,_) -> a == fst (head xs)) xs of
  (as, rest) -> (fst $ head xs, sort (map snd as)) : splitter rest

ioTestWebData = ["Facebook", "Google", "Google", "Facebook", "Youtube", "Twitter", "Twitter", "Facebook", "Google", "Google", "Facebook", "Twitter"]
testUniqueWebsites = testFunEq sort uniqueWebsites [(webData, ["Google","Facebook","Youtube"]), (ioTestWebData, ["Facebook", "Google", "Youtube", "Twitter"])]
testCount = testFun2 testFun count [("Google", webData, 2), ("Youtube", webData, 4), ("Facebook", webData, 3), ("Fake", webData, 0), ("Facebook", ioTestWebData, 4), ("Google", ioTestWebData, 4), ("Youtube", ioTestWebData, 1), ("Twitter", ioTestWebData, 3)]
testResult = testFunEq splitter result [(webData, [(4,"Youtube"),(3,"Facebook"),(2,"Google")]), (ioTestWebData, [(4,"Google"),(4,"Facebook"),(3,"Twitter"),(1,"Youtube")])]

ioTestWebDataDuration :: [(String, Float)]
ioTestWebDataDuration = [("Facebook", 24.3), ("Facebook", 22.2), ("Twitter", 32.2), ("Youtube", 12.3), ("Youtube", 53.3), ("Twitter", 22.3)]
testUniqueWebsites2_Double = testFunEq sort uniqueWebsites2 [(webDataDuration, ["Google", "Facebook", "Youtube"])]
testUniqueWebsites2_Float = testFunEq sort uniqueWebsites2 [(ioTestWebDataDuration, ["Facebook", "Twitter", "Youtube"])]
testCount2_Double = testFun2 testFunFract count2 [("Google", webDataDuration, 57.5), ("Facebook", webDataDuration, 60.9), ("Youtube", webDataDuration, 52.8), ("Fake", webDataDuration, 0.0)] 
testCount2_Float = testFun2 testFunFract count2 [("Youtube", ioTestWebDataDuration, 65.6), ("Twitter", ioTestWebDataDuration, 54.5), ("Facebook", ioTestWebDataDuration, 46.5)]
testResult2_WebData = testFunEq splitter (result2 count uniqueWebsites) [(webData, [(4,"Youtube"),(3,"Facebook"),(2,"Google")]), (ioTestWebData, [(4,"Google"),(4,"Facebook"),(3,"Twitter"),(1,"Youtube")])]

testFunResult2 f = all (\(dataset, idx, count, website) -> ((abs (fst (f dataset !! idx)) - count) < 1e-6 && (snd (f dataset !! idx)) == website) || error ("on input "++(show dataset)++".\n The result ranked on position "++(show idx)++ " is "++(show (f dataset !! idx))++" but should be "++(show (count, website))))
testResult2_Double = testFunResult2 (result2 count2 uniqueWebsites2) [(webDataDuration, 0, 60.9, "Facebook"), (webDataDuration, 1, 57.5, "Google"), (webDataDuration, 2, 52.8, "Youtube")]
testResult2_Float = testFunResult2 (result2 count2 uniqueWebsites2) [(ioTestWebDataDuration, 0, (65.6 :: Float), "Youtube"), (ioTestWebDataDuration, 1, (54.5 :: Float), "Twitter"), (ioTestWebDataDuration, 2, (46.5 :: Float), "Facebook")]

testAllWeb = testUniqueWebsites && testCount && testResult && testUniqueWebsites2_Double && testUniqueWebsites2_Float && testCount2_Double && testCount2_Float && testResult2_WebData && testResult2_Double && testResult2_Float 


--- test part 2
test_list f g ss st = all (test_fun f g) ss where
  test_fun f g s = f s == g s ||
    error ("function " ++ st ++ " on input " ++ show s ++ " delivered " 
      ++ show (g s) ++ ", but expected was " ++ show (f s))

test_map = test_list (all odd) (all_fold odd)ss "test_even" where 
  ss = [[2,3,4], [2, 9, 7],[6,6,6,6],[8,2,5,1,3,5],[2,12,80,12,66,100]]

test_map1 = test_list (all (>3)) (all_fold (>3))ss "test_even" where 
  ss = [[2,3,4], [2, 9, 7],[6,6,6,6],[8,2,5,1,3,5],[2,12,80,12,66,100]]


test_mapchar = test_list (all (<='A')) (all_fold (<='A'))ss "test_even" where 
  ss = [['a','B','d'], ['Z', 'd', 'f'],['z','a','b','o'],['A','P','K','L', 'M','E'],['w','q','g','n','e','t']]

test_digit = test_list dig2int dig2int_fold ss "test_even" where 
  ss = [[2,3,4], [2, 9, 7],[6,6,6,6],[8,2,5,1,3,5],[2,1,8,1,6,0]]

testfold = test_map&&test_map1&&test_mapchar &&test_digit

--- test part 3
test_number = test_list number number' tests "number"
  where number' xs = [ (i, xs !! i) | i <- [0 .. length xs - 1]]
        tests = ["hello", "", "world!!!"] 
 
testevenProd = test_list evenProdSum (evenProdSum' 2) tests "evenProdSum" where
  tests = [[], [5], [8493,49,1,3,2], replicate 15 6, replicate 16 6]
  evenProdSum' _ [] = 0
  evenProdSum' _ [_] = 0
  evenProdSum' n (_ : x : xs) = n * x + evenProdSum' (n+2) xs

test_lc = test_number && testevenProd

testAll = testfold && testAllWeb && test_lc