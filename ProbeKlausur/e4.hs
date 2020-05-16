fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)

main :: IO ()
main = do
  putStrLn "Type in your number: "
  number <- getLine
  let num = read number
  if num > 0 then
    do
      putStrLn $ "Fact of " ++ show num ++ ": " ++ show (fact num)
      main
  else
    putStrLn "BYE"