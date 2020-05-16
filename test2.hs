import Data.List

type First_Name = String
type Last_Name = String
type Country = String

data PersonT = Person First_Name Last_Name Country

newborns_2019 :: [PersonT]
newborns_2019 = [
    Person "Rainer" "Unsinn" "Germany",
    Person "Rainer1" "Unsinn" "Germany",
    Person "Rainer" "Unsinn" "Germany",
    Person "Rainer2" "Unsinn" "Germany",
    Person "Rainer2" "Unsinn" "Germany",
    Person "Rainer2" "Unsinn" "Germany"
  ]

all_names :: Country -> [First_Name]
all_names c = 
  [y | x <- newborns_2019, let y = fn x, cn x == c] 
  where
    fn (Person f _ _) = f
    cn (Person _ _ c) = c

nub1 :: Eq a => [a] -> [a]
nub1 = foldr (\x y -> if x `elem` y then y else x : y) []

nub2 :: Eq a => [a] -> [a]
nub2 = foldr (\x y -> x : filter (/=x) y) []

names :: First_Name -> Country -> Int
names n c = length $ (filter (\x -> x == n)) (all_names c)

tp :: Country -> [(Int, First_Name)]
tp c = sortBy (flip compare) $ map (\x -> (names x c, x)) (nub1 (all_names c))

preferred_names :: Country -> [First_Name]
preferred_names c = map snd (tp c) 


fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n-1)

main :: IO ()
main = do
  putStrLn "Please enter a number: "
  str <- getLine
  let number = (read str :: Integer)
  if (number > 0) then
    do
      putStrLn $ "Fact of " ++ str ++ " is " ++ show (fact number)
      main
  else
    do
      putStrLn "BYE"
      return ()