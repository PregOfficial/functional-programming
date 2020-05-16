{- Exercise 4.1 -}

-- 1) using if-then-else, and, or, not
boolFunIf :: Bool -> Bool -> Bool -> Bool
boolFunIf x y z = if(y && not z || not x && y || x && not y && z) then True else False

boolFunAon :: Bool -> Bool -> Bool -> Bool
boolFunAon x y z = y && not z || not x && y || x && not y && z

-- 3) using pattern matching
boolFunPm :: Bool -> Bool -> Bool -> Bool
boolFunPm False True _ = True
boolFunPm _ False False = False
boolFunPm True False True = True
boolFunPm True True False = True
boolFunPm _ _ _ = False

{- Exercise 4.2 -}

-- 4.2.1
--
-- define this datatype and the functions below
data Party = Oevp | Spoe | Fpoe | Gruene | Neos

mps :: Party -> Integer
mps Oevp = 71
mps Spoe = 40
mps Fpoe = 31
mps Gruene = 26
mps Neos = 15

coalition :: Party -> Party -> Bool
coalition x y = (mps x + mps y) >= 92

-- 4.2.2

-- define this datatype and the functions below
data Season = Spring | Summer | Fall | Winter 

-- define the function and write the type signature
-- pattern matching
daysInSeasonPM :: Season -> Integer
daysInSeasonPM Spring = 93
daysInSeasonPM Summer = 94
daysInSeasonPM Fall = 90
daysInSeasonPM Winter = 89

instance Eq Season where
  Spring == Spring = True
  Summer == Summer = True
  Fall == Fall = True
  Winter == Winter = True
  _ == _ = False

-- if-then-else
daysInSeasonITE :: Season -> Integer
daysInSeasonITE s = if(s == Spring) then 93 else if(s == Summer) then 94 else if(s == Fall) then 90 else if(s == Winter) then 89 else 0


{- Exercise 4.3 -}

-- 4.3.1

-- define the function and write the type signature
threeEqual :: Eq a => a -> a -> a -> Bool
threeEqual x y z = x == y && x == z


-- 4.3.2

foo :: a -> a -> a
foo x y = x

bar :: a -> a -> a
bar x y = y


{- Tests -}
{- You don't have to understand the Haskell-code in the tests,
   but you can just invoke them after having implemented
   some exercises -}

testData :: [((Bool, Bool, Bool), Bool)]
testData =
  [ ((False, False, False), False)
  , ((False, False, True) , False)
  , ((False, True, False) , True)
  , ((False, True, True)  , True)
  , ((True, False, False) , False)
  , ((True, False, True)  , True)
  , ((True, True, False)  , True)
  , ((True, True, True)   , False)
  ]

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

testBool :: (Bool -> Bool -> Bool -> Bool) -> Bool
testBool f = all (\t -> (snd t ==) . uncurry3 f $ fst t) testData

testBoolIf, testBoolAon, testBoolPm :: Bool
testBoolIf = testBool boolFunIf || error "test for boolFunIf failed"
testBoolAon = testBool boolFunAon || error "test for boolFunIf failed"
testBoolPm = testBool boolFunPm || error "test for boolFunIf failed"

testThreeEqual :: Bool
testThreeEqual =
  threeEqual 'a' 'a' 'a'
    && not (threeEqual True False True)
    && not (threeEqual 2 1 1)
    && not (threeEqual 1 1 2)
    && not (threeEqual 1 2 2)
    && not (threeEqual 2 1 2)
    && not (threeEqual 2 2 1)
    && not (threeEqual "a" "b" "c")
    || error "test for threeEqual failed"

testFooBar = foo 1 2 /= bar 1 2 && foo True False /= bar True False

testAll :: Bool
testAll = testBoolIf && testBoolAon && testBoolPm && testThreeEqual && testFooBar