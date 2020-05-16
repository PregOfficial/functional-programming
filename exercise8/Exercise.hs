{- 8.1 higher order functions -}

code :: Char -> String
code 'a' = "01"
code 'b' = "1"
code 'c' = "001"
code 'd' = "0001"

code2 :: Char -> String
code2 'a' = "11"
code2 'b' = "101"
code2 'c' = "01"
code2 'd' = "0011"

isPrefix, neitherPrefix :: String -> String -> Bool
isPrefix s t = null s || (not (null t) && head s == head t && isPrefix (tail s) (tail t))
neitherPrefix s t = not (isPrefix s t) && not (isPrefix t s)

{- TODO change this function and the type signature -}
encode :: (Char -> String) -> String -> String
encode codeFunction = 
  encode' where 
    encode' s
      | null s = "" 
      | otherwise = codeFunction (head s) ++ encode' (tail s)

{- TODO change this function and the type signature -}
decode :: (Char -> String) -> String -> String
decode codeFunction = 
  decode' where
    decode' s
      | null s = "" 
      | otherwise = 
        [char]++ decode' (drop (length (codeFunction char)) s) where
          char = aux 'a' 'd' s
          aux c d s
            | (isPrefix (codeFunction c) s) = c 
            | (c == d) = error ("not decodable:"++s) 
            | otherwise = aux (succ c) d s

{- TODO write this function and the corresponding type signature -}
compareCodes :: (Char -> String) -> (Char -> String) -> String -> (Bool, String, String)
compareCodes codeFunction1 codeFunction2 =
  compareCodes' where
    compareCodes' s =
      let
        encoded1 = encode codeFunction1 s
        encoded2 = encode codeFunction2 s
        in
          (length encoded1 < length encoded2, encoded1, encoded2)

{-
TODO find Strings (length at least 5) for which code and code2 respectively generate shorter encodings, using compareCodes

|         String |            code  |              code2 | code1 better |
|          ababa |         01101101 |       111011110111 | TRUE         |
|         abdadc | 0110001010001001 |  11101001111001101 | TRUE         |
|          caccd |  001010010010001 |       011101010011 | FALSE        |
|          cadaa |    0010100010101 |       011100111111 | FALSE        |
-}
            



{- TODO write this function and the corresponding type signature -}
nTimes :: (a -> a) -> Integer -> a -> a
nTimes f n x
  | n == 1 = f x
  | otherwise = f (nTimes f (n-1) x)

{- 8.2.1 type signatures partial application -}

div1 :: (Fractional a) => a -> a -> a
div1 = (/)

{-
div1:
  - divides 2 given Fractionals and returns a Fractional
-}

div2 :: (Fractional a) => a -> a
div2 = (2/)

{-
div2:
  - divides 2 by a given Parameter and returns a Value with the Type Fractional
-}

div3 :: (Fractional a) => a -> a
div3 = (/2)

{-
div3:
  - divides a given Parameter by 2 and returns a Value with the Type Fractional

difference between div2 and div3:
  - div2 4 = 0.5
  - div3 4 = 2
  - The difference is, that div2 divides 2 by 4 and
    div3 divides 4 by 2
-}

eqTuple :: (Eq b) => (a -> b) -> (a, a) -> Bool
eqTuple f = (\(x,y) -> f x == f y)

eqTuple' :: (Eq b) => (a -> b) -> (a, a) -> Bool
eqTuple' f (x, y) = f x == f y

{-
eqTuple and eqTuple' are the same, because in eqTuple the function is called with a lambda expression
and the eqTuple calls it directly with the values of the tuple
-}

{-
div1 4 2 = 2

foo1 x y = y / x
  - can't be the same as div1 x y, because foo1 divides x by y and foo1 divides y by x
  - foo1 4 2 = 0.5
foo2 x y = (\u v -> v / u) y x
  - is the same because you create a anonymous function with the values u and v.
    Then it's dividing v by u. The value for u is y and for v it's x => x / y
  - foo2 4 2 = 2
-}

{-
8.3.1

a) type-correct
b) type-correct
c) type-correct
d) not type-correct
e) not type-correct

8.3.2
b and c ist the same as f (g (h (i (x))))

-}
{- 8.3.2 drop parenthesis -}
append :: [a] -> [a] -> [a]
append = (++)

not0 :: (Eq a, Num a) => a -> Bool
not0 = (/=0)

foo x = not0 . head $ tail $ tail $ x

foo1 x (y, z) = not0 . head $ append y $ append (tail x) z

{- tests -}

testNTimes = nTimes id 100 5 == 5 && nTimes (+1) 100 5 == 105

testEx81 = testNTimes && all ((\(x,_,_)->x) . compareCodes code code2) ["b", "ab", "bc"]

testAll = testEx81