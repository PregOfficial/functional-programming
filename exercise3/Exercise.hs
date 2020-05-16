{- Exercise 3.2 -}

-- version where recursion counts downwards until 0
sum_down :: Integer -> Integer
sum_down n = 
  if (n < 1) then 0
  else n + sum_down(n - 1)

-- version where recursion counts upwards until n
sum_up :: Integer -> Integer
sum_up n = 
  if(n < 1) then 0
  else sum_up_aux n 0 

sum_up_aux :: Integer -> Integer -> Integer
sum_up_aux n x =
  if (n == x) then x
  else x + sum_up_aux n (x+1)

-- any solution
fib :: Integer -> Integer
fib n = 
  if (n == 0) then 0
  else if (n == 1) then 1
  else fib (n-1) + fib (n-2)

-- efficient version
fib_eff :: Integer -> Integer
fib_eff n = 
  if(n == 0) then 0
  else if (n == 1) then 1
  else fib_eff_aux 0 1 (n - 1)

fib_eff_aux :: Integer -> Integer -> Integer -> Integer
fib_eff_aux x y c =
  if (c == 0) then y
  else fib_eff_aux y (x + y) (c-1)
  
{- Exercise 3.3 -}

isDivisible :: Integer -> Integer -> Bool
isDivisible x y = (x == (x `div` y) * y)

isPrime :: Integer -> Bool
isPrime n =
  if(n == 2) then True
  else if (n > 2)
    then prime_aux n (n-1)
  else False

prime_aux :: Integer -> Integer -> Bool
prime_aux n c =
  if(c == 1) then True
  else 
    if(isDivisible n c == False) then prime_aux n (c-1)
    else False

gcd_function :: Integer -> Integer -> Integer
gcd_function x y =
  if(x == y) then x
  else if(x > y) then gcd (x - y) y
  else gcd (y - x) x

{- Tests -}
{- You don't have to understand the Haskell-code in the tests,
   but you can just invoke them after having implemented 
   some exercises -}

-- tests for sum
sums = [0,1,3,6,10,15,21,28,36,45,55]
test_sum_down = map sum_down [0..10] == sums 
  || error "test failed on sum_down"
test_sum_up   = map sum_up [0..10] == sums 
  || error "test failed on sum_up"
test_sum = test_sum_up && test_sum_down

-- tests for fib
fibs = [0,1,1,2,3,5,8,13,21,34]
test_fib = map fib [0 .. 9] == fibs 
  || error "test fib failed"
test_fib_eff = map fib_eff [0 .. 9] == fibs 
  || error "test fib_eff failed"
test_fib_both = test_fib && test_fib_eff

-- tests for primes
primes = [2,3,5,7,11,13,17,19]
test_primes = filter isPrime [0..20] == primes 
  || error "test_primes failed"

-- test for all exercises
test_all = test_primes && test_sum && test_fib_both