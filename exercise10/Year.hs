module Year(Month, Year, monthInfo2) where

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

  monthInfo2 :: Month -> Year -> (Int, Int, String, Int)
  monthInfo2 m y = (fstdays y !! (m - 1), mlengths y !! (m - 1),header,7) where
    header = " Mo Tu We Th Fr Sa Su"