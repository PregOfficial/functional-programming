import Data.List(sortBy)

type First_Name = String
type Last_Name = String
type Country = String

data PersonT = Person First_Name Last_Name Country

newborns_2019 :: [PersonT]
newborns_2019 = [
  Person "Rainer" "Unsinn" "Germany",
  Person "Udo" "Friday" "USA",
  Person "Udo" "Friday" "USA",
  Person "Gladys" "Friday" "USA",
  Person "Hans" "Friday" "USA",
  Person "Gladys" "Friday" "USA",
  Person "Gladys" "Friday" "USA"
  ]

all_names :: Country -> [First_Name]
all_names c = [getFirstName x | x <- filter (\x -> (getCountry x) == c) newborns_2019]

getFirstName :: PersonT -> First_Name
getFirstName (Person x y z) = x

getCountry :: PersonT -> Country
getCountry (Person x y z) = z

fnr :: (Eq a) => a -> [a] -> [a]
fnr x [] = [x]
fnr x xs
  | x == head xs = xs
  | otherwise = x:xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub ys = foldr fnr [] ys

preferred_names :: Country -> [First_Name]
preferred_names country = nub $ sortBy () (all_names country)