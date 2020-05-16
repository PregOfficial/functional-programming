{- Exercise 2.2 -}

-- values from the exercise sheet
baseAttack :: Integer
baseAttack = 4
baseDefense :: Integer
baseDefense = 3

calculateDefense :: Integer -> Integer
calculateDefense armorDefense = baseDefense * armorDefense

calculateAttack :: Integer -> Integer
calculateAttack weaponDamage = baseAttack + weaponDamage

-- main function: calculateDamage

calculateDamage :: Integer -> Integer -> Integer
calculateDamage armorDefense weaponDamage = 
  let damage = calculateDefense armorDefense - calculateAttack weaponDamage
  in if damage < 0
    then 0
  else
    damage


{- Exercise 2.3 -}

square :: Integer -> Integer
square n = if n > 0 then square (n - 1) + 2*n - 1 else 0

-- invoke test_square in ghci
test_square = if square 3 == 3^2 then True else error "test on square failed"

-- factorial from the lecture
fact :: Integer -> Integer
fact n = if n > 0 then n * fact (n - 1) else 1

binom_fd :: Integer -> Integer -> Integer
binom_fd n k = fact n `div` (fact k * fact (n - k))

binom_dc :: Integer -> Integer -> Integer
binom_dc n k = if n == k then 1 else if n < 0 || k < 0 then 0 else binom_dc (n-1) k + binom_dc (n - 1) (k - 1)

binom_3 :: Integer -> Integer -> Integer
binom_3 n k = if (k == 0 || n == k) then 1 else if n < 0 || k < 0 then 0 else (n * binom_3 (n - 1) (k - 1)) `div` k

-- tests for binomials 
test_binom_fd = if binom_fd 3 2 == 3 then True else error "test on binom_fd failed"
test_binom_dc = if binom_dc 3 2 == 3 then True else error "test on binom_dc failed"

-- a test for all parts of exercise 2.3: invoke test_ex_2_3 in ghci
test_ex_2_3 = test_square && test_binom_fd && test_binom_dc