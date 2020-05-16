module Picture(Picture, info2Pic, showPic) where

  type Height  = Int
  type Width   = Int
  type Picture = (Height, Width, [[Char]])

  above :: Picture -> Picture -> Picture
  (h, w, css) `above` (h', w', css')
    | w == w'   = (h + h', w, css ++ css')
    | otherwise = error "above: different widths"

  stack :: [Picture] -> Picture
  stack = foldr1 above

  row :: String -> Picture
  row r = (1, length r, [r])

  showPic :: Picture -> String
  showPic (_, _, css) = unlines css

  -- groupsOfSize splits list into sublists of given length
  groupsOfSize :: Int -> [a] -> [[a]]
  groupsOfSize n xs =
    if null ys then []
    else ys : groupsOfSize n zs
    where
      (ys, zs) = splitAt n xs

  rjustify :: Int -> String -> String
  rjustify n xs
    | l <= n = replicate (n - l) ' ' ++ xs
    | otherwise = error ("text \"" ++ xs ++ "\" too long")
    where l = length xs

  info2Pic :: (Int, Int, String, Int) -> Picture
  info2Pic (offset, n, h, s)
    | length h /= s * 3 = error "length of h not s * 3"
    | otherwise = 
      row h `above` 
        stack (map row (groupsOfSize (s*3) 
          $ createMissingBlanks (s*3) (replicate (offset*3) ' ' 
            ++ concatMap (rjustify 3 . show) [1 .. n])))

  createMissingBlanks :: Int -> String -> String
  createMissingBlanks l s
    | length s `mod` l == 0 = s
    | otherwise = 
        let diff = (length s `mod` l) 
        in
        s ++ replicate (l - diff) ' '
