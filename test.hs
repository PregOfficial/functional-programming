data Coin = OneCent | TenCent | TwoEuros

value :: Coin -> Float
value OneCent = 0.01
value TenCent = 0.1
value TwoEuros = 2.0

instance Show Coin where
  show OneCent = show "1 cent"
  show TenCent = show "10 cents"
  show TwoEuros = show "2 EUR"

total :: [Coin] -> Float
total coins
  | null coins = 0
  | otherwise = value (head coins) + total (tail coins)