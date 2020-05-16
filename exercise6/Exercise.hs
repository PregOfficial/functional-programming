{- Rational Numbers -}
import Data.Ratio

data Rat = RatC Integer Integer deriving Show

{- 4.2.1: Finish Rational Number implementation and modify the implementation to perform normalization -}

instance Eq Rat where
  (RatC n1 d1) == (RatC n2 d2) = (n1 * d2 == n2 * d1)

instance Num Rat where
  (RatC n1 d1) * (RatC n2 d2) = lcd(RatC (n1 * n2) (d1 * d2))
  fromInteger i = RatC i 1
  abs (RatC n d) = RatC (abs n) (abs d)
  
  (RatC n1 d1) + (RatC n2 d2) = lcd (RatC (n1 * d2 + n2 * d1) (d1 * d2))
  (RatC n1 d1) - (RatC n2 d2) = lcd (RatC (n1 * d2 - n2 * d1) (d1 * d2))
  negate (RatC n d) = RatC(-n) d
  signum (RatC n d) = if(n > 0) then 1 else if(n < 0) then -1 else 0

lcd :: Rat -> Rat
lcd (RatC n d) = if(d > 0) then RatC (n `div` (gcd n d)) (d `div`(gcd n d)) else error "Error"

{- 4.2.2: Finish mySqrt from the slides with the appropriate type annotation and test it using Rat -}

approx :: (Ord a, Fractional a) => a -> a -> a
approx n x = if goodEnough n x
  then x
  else approx n (improve n x)

goodEnough :: (Ord a, Fractional a) => a -> a -> Bool
goodEnough n x = abs (x^2 - n) < 0.0000001

improve :: (Fractional a) => a -> a -> a
improve n x = (x + n / x) / 2

mySqrt :: (Ord a, Fractional a) => a -> a
mySqrt n = if n >= 0
  then approx n n
  else error "sqrt invoked on negative number"

-- mySqrt 2 :: Rat

{- 4.2.3: Finish missing instances to Rat -}
instance Ord Rat where
  compare (RatC n1 d1) (RatC n2 d2) = compare(n1 * d2) (n2 * d1)

instance Fractional Rat where
  fromRational a = RatC (numerator a) (denominator a)
  (RatC n1 d1) / (RatC n2 d2) = lcd (RatC(n1*d2) (n2*d1))






{- Currency exchange -}

data Pair a b = Pair_C a b deriving (Show, Eq)
data Triple a b c = Triple_C a b c deriving (Show, Eq)

data Cash = Currency Integer Double deriving (Show, Eq)

{- Exercise 4.3.1 -}

valid_cash :: Cash -> Cash -> Maybe (Triple Cash Integer Double)
valid_cash (Currency budget a) (Currency atleast_exchange b)
  | ((fromIntegral budget) * a > (fromIntegral atleast_exchange) * b && budget > 0 && a > 0 && atleast_exchange > 0 && b > 0 && (fromIntegral atleast_exchange) * b > 0) = Just (Triple_C (Currency budget a) atleast_exchange b) 
  | otherwise = Nothing

{- 4.3.2  Exchange the budget for the targeted currency -}

exchange :: Maybe (Triple Cash Integer Double) -> Maybe (Pair Cash Cash)
exchange (Just (Triple_C (Currency budget a) atleast_exchange b)) = Just (Pair_C (Currency atleast_exchange b) (calcBudget (Currency budget a) (Currency atleast_exchange b)))
exchange (Nothing) = Nothing

calcBudget :: Cash -> Cash -> Cash
calcBudget (Currency budget a) (Currency atleast_exchange b) = Currency (round (((fromIntegral budget) * a - (fromIntegral atleast_exchange) * b) / a)) a

--- 4 Exchange function
exchange_currency:: Cash -> Cash -> Maybe (Pair Cash Cash)
exchange_currency a b = exchange (valid_cash a b)






{- Tests: just execute test_rat_num, test_rat_sqrt, test_rat_fractional test_rat_ord
   you don't have to understand the definitions -}
test_list f s as bs ys = all (test_fun f s) (zip (zip as bs) ys) where
  test_fun f s ((a, b), y) = f a b == y ||
    error ("function " ++ s ++ " on inputs " ++ show a ++ " and " ++ show b ++ " delivered " 
      ++ show (f a b) ++ ", but expected was " ++ show y)

test_list_norm f s as bs ys = all (test_fun f s) (zip (zip as bs) ys) where
  test_fun f s ((a, b), y) = show (f a b) == show y ||
    error ("function " ++ s ++ " on inputs " ++ show a ++ " and " ++ show b ++ " delivered " 
      ++ show (f a b) ++ ", but expected was " ++ show y)

as = [RatC 1 2, RatC 4 5, RatC 3 8, RatC 1 10, (fromInteger 1 :: Rat)]
bs = [RatC 1 3, (fromInteger 2 :: Rat), RatC 2 5, RatC 1 11, (fromInteger 2 :: Rat)]
adds = [RatC 5 6, RatC 14 5, RatC 31 40, RatC 21 110, RatC 3 1]
subs = [RatC 1 6, RatC (-6) 5, RatC (-1) 40, RatC 1 110, RatC (-1) 1]
mults = [RatC 1 6, RatC 8 5, RatC 3 20, RatC 1 110, RatC 2 1]

test_rat_num_ops = test_list (+) "(+)" as bs adds && test_list (-) "(-)" as bs subs && test_list (*) "(*)" as bs mults
test_rat_fromInteger = (fromInteger 4 :: Rat) == (RatC 4 1) || error ("function fromInteger on input 4 delivered " ++ show (fromInteger 4 :: Rat) ++ ", but expected was RatC 4 1")
test_rat_signum_abs = all (test_fun signum "signum") (zip xs sigs) && all (test_fun abs "abs") (zip xs abss) where
  xs = [RatC (-2) 1, RatC 1 1, RatC (-1) 2, RatC (-1) 10]
  sigs = [RatC (-1) 1, RatC 1 1, RatC (-1) 1, RatC (-1) 1]
  abss = [RatC 2 1, RatC 1 1, RatC 1 2, RatC 1 10]
  test_fun f s (x, y) = f x == y ||
    error ("function " ++ s ++ "on input " ++ show x ++ " delivered " ++ show (signum x) ++ ", but expected was " ++ show y)
test_rat_num = test_rat_num_ops && test_rat_fromInteger && test_rat_signum_abs

test_rat_norm = test_list_norm (+) "(+)" as bs adds && test_list_norm (-) "(-)" as bs subs && test_list_norm (*) "(*)" as bs mults

sts = [False, True, True, False, True]
test_rat_ord = test_list (<) "(<)" as bs sts

divs = [RatC 3 2, RatC 2 5, RatC 15 16, RatC 11 10, RatC 1 2]
test_rat_fract_div = test_list (/) "(/)" as bs divs
test_rat_fract_norm = test_list_norm (/) "(/)" as bs divs
test_rat_fract = test_rat_fract_div && test_rat_fract_norm

test_rat_sqrt = case mySqrt 2 of 
  RatC n d -> n == 665857 && d == 470832 
     || error ("function mySqrt 2 :: Rat returned " ++ show n ++ " / " ++ show d ++ ", but expected 665857 / 470832")

--- Testing exercise Maybe


test_valid = test_list valid_cash "valid_cash" as bs ys where
   ys = [Nothing,Nothing,Nothing,Nothing,Nothing,Just (Triple_C (Currency 3100 1.1) 102 12.1)]
   as = [Currency 120 (-1), Currency 12 12.1, Currency 82 2.1, Currency 155 0.8, Currency (-3100) 1.1, Currency 3100 1.1]
   bs = [Currency 20 3.5, Currency (-52) 1.2, Currency 120 2.1, Currency 189 (-0.9), Currency 102 12.1, Currency 102 12.1]

test_exchange = test_list exchange_currency "exchange_currency" ass bss yss where
   yss = [Nothing,Just (Pair_C (Currency 120 1.2) (Currency 1188 12.0)),
      Just (Pair_C (Currency 100 2.0) (Currency 720 2.0)),
      Nothing,
      Just (Pair_C (Currency 102 0.8) (Currency 232 1.2)),
      Just (Pair_C (Currency 102 12.0) (Currency 2284 1.5))]
   ass = [Currency 120 1, Currency 1200 12, Currency 820 2, Currency 155 0.8, Currency 300 1.2, Currency 3100 1.5]
   bss = [Currency 200 3.5, Currency 120 1.2, Currency 100 2, Currency 189 0.9, Currency 102 0.8, Currency 102 12]


test_rat = test_rat_num && test_rat_norm && test_rat_ord && test_rat_fract && test_rat_sqrt

test_all = test_rat && test_valid && test_exchange
