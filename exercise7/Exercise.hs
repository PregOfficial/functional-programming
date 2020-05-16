{- 7.1.1 define datatypes for our points in the coordinate systems -}

data Polar = PolarC Double Double
data Cart = CartC Double Double

createPolar :: Double -> Double -> Polar
createPolar r phi = PolarC r ((phi*pi)/180)

{- 7.1.2 Write a function which converts Cart into a Tuple -}
cart2Tuple :: Cart -> (Double, Double)
cart2Tuple (CartC x y) = (x, y)

polar2Tuple :: Polar -> (Double, Double)
polar2Tuple (PolarC r phi) = (r, phi)


{- 7.1.3 define conversion functions between the coordinate systems -}

polar2Cart :: Polar -> Cart
polar2Cart (PolarC r phi) = CartC (roundDouble (r*(cos phi))) (roundDouble (r*(sin phi)))

cart2Polar :: Cart -> Polar
cart2Polar (CartC x y) = 
  let r = sqrt (x^^2 + y^^2)
  in PolarC r (calcPhi x y r)

roundDouble :: Double -> Double
roundDouble x = fromIntegral (round (x*10))/10

calcPhi :: Double -> Double -> Double -> Double
calcPhi x y r
  | y >= 0 && r /= 0 = acos (x/r)
  | y < 0 = -acos (x/r)
  | otherwise = 0

{- 7.1.4 write a Show instance for our datatypes -}

instance Show Polar where
  show p = show (cart2Tuple (polar2Cart p))

instance Show Cart where
  show c = show (cart2Tuple c) 

{- 7.2 -}

code :: Char -> String
code 'a' = "01"
code 'b' = "1"
code 'c' = "001"
code 'd' = "0001"

{- 7.2.1 -}

encode :: String -> String
encode s = if null s 
  then "" 
  else (code (head s) ++ encode (tail s))

{- 7.2.2 -}

isPrefix, neitherPrefix :: String -> String -> Bool
isPrefix s t = checkPrefix s "" t
neitherPrefix s t = not (checkPrefix s "" t || checkPrefix t "" s)

checkPrefix :: String -> String -> String -> Bool
checkPrefix prefix s input =
  if (prefix == s) then True
  else if (null input) then False
  else 
    checkPrefix prefix (s ++ [head input]) (tail input)

{- 7.2.3 -}

decode :: String -> String
decode s 
    | isPrefix (code 'a') s = "a" ++ decode(drop 2 s)
    | isPrefix (code 'b') s = "b" ++ decode(drop 1 s)
    | isPrefix (code 'c') s = "c" ++ decode(drop 3 s)
    | isPrefix (code 'd') s = "d" ++ decode(drop 4 s)
    | s == "" = s
    | otherwise = "ERROR can\'t decode String"

{- 7.2.4 -}

isPrefixCode :: Bool
isPrefixCode = 
  neitherPrefix (code 'a') (code 'b') &&
  neitherPrefix (code 'a') (code 'c') &&
  neitherPrefix (code 'a') (code 'd') &&
  neitherPrefix (code 'b') (code 'c') &&
  neitherPrefix (code 'b') (code 'd') &&
  neitherPrefix (code 'c') (code 'd')

data A = B (Double, Integer, Integer) | C String (Double, Double) 
  deriving (Show, Eq)

instance Ord A where
  (B x) <= (C _ y) = True
  (C _ x) <= (B y) = False
  (B x) <= (B y) = x <= y
  (C s1 x) <= (C s2 y) = s1 < s2 || (s1 == s2 && x <= y)  


{- Tests -}

epsilon, radius, angle :: Double
epsilon = 0.001
radius = sqrt 2
angle  = 45

polar :: Polar
polar = createPolar radius angle

cart :: Cart
cart  = polar2Cart $ polar

testCP = (\(r,a) -> r == radius && a == (angle / (180 / pi))) . polar2Tuple $ polar

diff (r, p) (r', p') =
  (abs $ r - r') < epsilon && (abs $ p - p') < epsilon

testConversionP = diff p mP
  where
    mP = polar2Tuple . cart2Polar . polar2Cart $ polar
    p  = polar2Tuple $ polar

testConversionC = diff c mC
  where
    mC = cart2Tuple . polar2Cart . cart2Polar $ cart
    c  = cart2Tuple $ cart

testConversionPT = diff (radius, angle / (180 / pi)) (polar2Tuple $ polar)

testConversionCT = diff (1.0, 1.0) (cart2Tuple $ cart)

testShow = (show cart) == (show polar)

{- Tests for exc 7.2 -}

testFun :: (Show a,Show b,Eq b) => (a -> b) -> [(a,b)] -> Bool
testFun f = all (\p -> f (fst p) == snd p || 
  error ("on input "++(show (fst p))++" output is "++(show (f (fst p)))++" but should be "++(show (snd p)))) 
testFun2 f l = testFun (\p -> f (fst p) (snd p)) (map (\(x,y,z) -> ((x,y),z)) l)
testFun0 f l = testFun (\() -> f) (map (\x -> ((),x)) l)
testEncode = testFun encode ioEncode
ioEncode = [("",""),("c","001"),("abcd","0110010001"),("dcba","0001001101")]
testIsPrefix = testFun2 isPrefix ioIsPrefix  
ioIsPrefix = [("","0",True),("1","",False),("1110","111",False),("00","0001",True),("aab","aaaab",False)]
testNeitherPrefix = testFun2 neitherPrefix ioNeitherPrefix
ioNeitherPrefix = [("","a",False),("000","00",False),("010","1",True),("001","0001",True)] 
testDecode = testFun decode ioDecode
ioDecode = [("0110010001","abcd"),("0001001101","dcba"),("010101","aaa"),("","")] 
testIsPrefixCode = testFun0 isPrefixCode ioIsPrefixCode
ioIsPrefixCode = [True]
testExc72 = testEncode && testIsPrefix && testNeitherPrefix && testDecode && testIsPrefixCode

{- Test all exercises -}

testAll = testConversionP && testConversionC && testConversionPT && 
          testConversionCT && testCP && testShow && testExc72
