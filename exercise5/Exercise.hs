{- Cities, States and Countries -}

data City = Innsbruck | Munich | Graz | Gramais
data Country = Austria | Germany
data State = Tyrol | Bavaria | Styria

class Info a where
  population :: a -> Integer
  area :: a -> Integer

{- TODO: Make City, Country and State instances of Info -}

instance Info City where
  population Innsbruck = 132110
  population Munich = 1471508
  population Graz = 288806
  population Gramais = 41
  area Innsbruck = 105
  area Munich = 105
  area Graz = 127
  area Gramais = 32

instance Info Country where
  population Germany = 83019213
  population Austria = 8858775
  area Germany = 357578
  area Austria = 83878

instance Info State where
  population Tyrol = 754705
  population Bavaria = 13076721
  population Styria = 1243052
  area Tyrol = 12640
  area Bavaria = 70550
  area Styria = 16401

{- TODO: Define the following functions -}

inhabitantsPerSkm :: (Info a) => a -> Integer
inhabitantsPerSkm info = population info `div` area info

hasMoreArea :: (Info a, Info b) => a -> b -> Bool
hasMoreArea infoX infoY = area infoX > area infoY

hasMoreInhabitants :: (Info a, Info b) => a -> b -> Bool
hasMoreInhabitants infoX infoY = population infoX > population infoY

{- TODO: Make Country and State instances of Located -}

class Located a where
  isIn :: City -> a -> Bool

instance Located Country where
  isIn Innsbruck Austria = True
  isIn Graz Austria = True
  isIn Gramais Austria = True
  isIn Munich Germany = True
  isIn _ _ = False

instance Located State where
  isIn Innsbruck Tyrol = True
  isIn Graz Styria = True
  isIn Gramais Tyrol = True
  isIn Munich Bavaria = True
  isIn _ _ = False

{- Approximation of Pi -}

piAn :: (Fractional a, Num a) => a -> a
piAn n = 8 / ((4*n+1) * (4*n+3))

piRlAux :: (Fractional a, Ord a, Num a) => a -> a -> a
piRlAux current end = if(current > end) then 0 else piAn(current) + piRlAux(current + 1) end

-- adjust type and defining equation
pi_lr :: (Fractional a, Ord a, Num a) => a -> a
pi_lr k = if(k == 0) then piAn(k) else piAn(k) + pi_lr(k-1)

-- adjust type and defining equation
pi_rl :: (Fractional a, Ord a, Num a) => a -> a
pi_rl k = piRlAux 0 k


-- 5.3.3

piDoubleLr :: Double -> Double
piDoubleLr a = pi_lr a

piFloatLr :: Float -> Float
piFloatLr a = pi_lr a

piDoubleRl :: Double -> Double
piDoubleRl a = pi_rl a

piFloatRl :: Float -> Float
piFloatRl a = pi_rl a

{-

+---------+---------------------+---------------------+-----------+-----------+
| k       | Double Lr           | Double Rl           | Float Lr  | Float Rl  |
+---------+---------------------+---------------------+-----------+-----------+ 
| 1       | 2.895238095238095   | 2.895238095238095   | 2.8952382 | 2.8952382 |
+---------+---------------------+---------------------+-----------+-----------+
| 100     | 3.136642188870299   | 3.1366421888703013  | 3.1366425 | 3.1366422 |
+---------+---------------------+---------------------+-----------+-----------+
| 10000   | 3.1415426585893202  | 3.1415426585893242  | 3.141384  | 3.1415427 |
+---------+---------------------+---------------------+-----------+-----------+
| 1000000 | 3.141592153590402   | 3.141592153590293   | 3.141384  | 3.1415923 |
+---------+---------------------+---------------------+-----------+-----------+

-}

-- adjust defining equation
pi_opt :: (Eq a, Ord a, Fractional a) => a
pi_opt = piOptAux 0 1

piOptAux :: (Eq a, Ord a, Fractional a) => a -> a -> a
piOptAux a b = if(pi_lr a > pi_lr b) then piOptAux (a / 2) (b / 2) else if(pi_lr a == pi_lr b) then pi_lr a else piOptAux (a*2+1) (b*2+1)


-- Live Exercise

instance Num Bool where
  x + y = x || y
  x * y = x && y
  fromInteger 0 = False
  fromInteger 1 = True
  negate True = False
  negate False = True
  abs = undefined
  signum = undefined

{- Tests: just execute test_pi_lr, test_pi_rl, test_pi_opt, ,test_pi or test_locations
   you don't have to understand the definitions -}

test_list f s ys = all (test_fun f s) (zip test_nums ys) where
  test_nums = [0..10] ++ [1000..1010]
  test_fun f s (x, e) = abs (f x - e) < 0.0000001 ||
    error ("function " ++ s ++ " on input " ++ show x ++ " delivered " ++ show (f x) ++  
      ", but expected was " ++ show e)

test_pi_lr = test_list pi_lr "pi_lr" test_lr_f && test_list pi_lr "pi_lr" test_lr_d where
  test_lr_f = [2.6666667,2.8952382,2.9760463,3.017072,3.0418398,3.058403,3.0702548,3.0791535,3.0860798,3.0916238,3.0961616,3.141091,3.1410916,3.141092,3.1410925,3.141093,3.1410935,3.141094,3.1410944,3.141095,3.1410954,3.1410959] :: [Float]
  test_lr_d = [2.6666666666666665,2.895238095238095,2.976046176046176,3.017071817071817,3.041839618929402,3.0584027659273314,3.070254617779183,3.0791533941974256,3.0860798011238324,3.091623806667838,3.0961615264636406,3.141093153121444,3.1410936516248436,3.141094149134218,3.1410946456525384,3.141095141182763,3.141095635727838,3.1410961292906987,3.141096621874268,3.1410971134814583,3.141097604115169,3.1410980937782886] :: [Double]

test_pi_rl = test_list pi_rl "pi_rl" test_rl_f && test_list pi_rl "pi_rl" test_rl_d where
  test_rl_f = [2.6666667,2.8952382,2.9760463,3.017072,3.0418396,3.0584028,3.0702548,3.0791535,3.0860798,3.0916238,3.0961616,3.1410933,3.1410937,3.1410942,3.1410947,3.1410952,3.1410956,3.141096,3.1410966,3.141097,3.1410975,3.1410983] :: [Float]
  test_rl_d = [2.6666666666666665,2.895238095238095,2.976046176046176,3.017071817071817,3.041839618929402,3.058402765927332,3.0702546177791836,3.079153394197426,3.086079801123833,3.0916238066678385,3.096161526463641,3.141093153121449,3.1410936516248484,3.141094149134223,3.1410946456525437,3.141095141182768,3.1410956357278432,3.1410961292907036,3.141096621874273,3.141097113481463,3.1410976041151737,3.141098093778293] :: [Double]

test_pi_opt = let exp = 3.141384 :: Float in 
  pi_opt == exp || error ("evaluating pi_opt results in " ++ show pi_opt ++ ", but it should be computed as " ++ show exp)

test_pi = test_pi_lr && test_pi_rl && test_pi_opt

test_locations = hasMoreArea Austria Tyrol &&
                 inhabitantsPerSkm Gramais == 1 &&
                 hasMoreInhabitants Tyrol Gramais &&
                 not (hasMoreInhabitants Gramais Germany)

test_all = test_pi && test_locations