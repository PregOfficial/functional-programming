type Month   = Int
type Year    = Int
type Dayname = Int -- Mo = 0, Tu = 1, ..., So = 6

fstdays :: Year -> [Dayname]
fstdays = take 12 . map (`mod` 7) . mtotals
  where 
    mtotals :: Year -> [Int]
    mtotals y = scanl (+) (jan1 y) (mlengths y)

mlengths :: Year -> [Int]
mlengths y = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  where feb = if leap y then 29 else 28
  
leap :: Year -> Bool
leap y = if y `mod` 100 == 0 then y `mod` 400 == 0 else y `mod` 4 == 0

-- January 1 in year 1 was a Monday
jan1 :: Year -> Dayname
jan1 y = (365 * x + x `div` 4 - x `div` 100 + x `div` 400) `mod` 7
  where x = y - 1

monthInfo :: Month -> Year -> (Dayname, Int)
monthInfo m y = (fstdays y !! (m - 1), mlengths y !! (m - 1))


type Height  = Int
type Width   = Int
type Picture = (Height, Width, [[Char]])

above :: Picture -> Picture -> Picture
(h, w, css) `above` (h', w', css')
  | w == w'   = (h + h', w, css ++ css')
  | otherwise = error "above: different widths"

stack :: [Picture] -> Picture
stack = foldr1 above

beside :: Picture -> Picture -> Picture
(h, w, css) `beside` (h', w', css')
  | h == h'   = (h, w + w', zipWith (++) css css')
  | otherwise = error "beside: different heights"

spread :: [Picture] -> Picture
spread = foldr1 beside

tile :: [[Picture]] -> Picture
tile = stack . map spread

pixel :: Char -> Picture
pixel c = (1, 1, [[c]])

row :: String -> Picture
row r = (1, length r, [r])

blank :: Height -> Width -> Picture
blank h w = (h, w, blanks)
  where
    blanks = replicate h (replicate w ' ')

daysOfMonth :: Month -> Year -> [Picture]
daysOfMonth m y =
  map (row . rjustify 3 . pic) [1 - d .. numSlots - d]
  where
    (d, t) = monthInfo m y
    numSlots = 6 * 7  -- max 6 weeks * 7 days per week
    pic n = if 1 <= n && n <= t then show n else ""
    
rjustify :: Int -> String -> String
rjustify n xs
  | l <= n = replicate (n - l) ' ' ++ xs
  | otherwise = error ("text \"" ++ xs ++ "\" too long")
  where l = length xs

month :: Month -> Year -> Picture
month m y = above weekdays . tile . groupsOfSize 7 $ daysOfMonth m y
  where weekdays = row " Mo Tu We Th Fr Sa Su"

-- groupsOfSize splits list into sublists of given length
groupsOfSize :: Int -> [a] -> [[a]]
groupsOfSize n xs =
  if null ys then []
  else ys : groupsOfSize n zs
  where
    (ys, zs) = splitAt n xs
    
showPic :: Picture -> String
showPic (_, _, css) = unlines css

showMonth :: Month -> Year -> String
showMonth m y = showPic $ month m y