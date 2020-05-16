{- logic of Connect Four -}
import Text.Read

type Tile   = Int   -- 0, 1, or 2
type Player = Int   -- 1 and 2
type Move   = Int   -- column number
data State = State Player [[Tile]] deriving (Show, Read) -- list of rows

empty :: Tile
empty = 0

num_rows, num_cols :: Int
num_rows = 6
num_cols = 7

start_player :: Player
start_player = 1

init_state :: Player -> State
init_state player = State player
  (replicate num_rows (replicate num_cols empty))

other_player :: Player -> Player
other_player = (3 -)

drop_tile :: Move -> State -> State
drop_tile col (State player rows) = State
  (other_player player)
  (reverse $ drop_aux $ reverse rows)
    where
      drop_aux (row : rows) =
        case splitAt col row of
         (first, i : last) ->
           if i == empty
             then (first ++ player : last) : rows
             else row : drop_aux rows

valid_moves :: State -> [Move]
valid_moves (State _ rows) =
  map fst . filter ((== empty) . snd) . zip [0..] $ head rows

show_player :: Player -> String
show_player 1 = "X"
show_player 2 = "O"

show_tile :: Tile -> Char
show_tile t = if t == empty then '.' else head $ show_player t

show_state :: State -> String
show_state (State player rows) =
  unlines $ map (head . show) [0 .. num_cols - 1] :
    map (map show_tile) rows
     ++ ["\nPlayer " ++ show_player player ++ " to go"]

transpose_rows ([] : _) = []
transpose_rows xs = map head xs : transpose_rows (map tail xs)

winning_row :: Player -> [Tile] -> Bool
winning_row player [] = False
winning_row player row = take 4 row == replicate 4 player
  || winning_row player (tail row)

winning_player :: State -> Maybe Player
winning_player (State player rows) =
  let oplayer = other_player player
      long_rows = rows ++ transpose_rows rows
    in if any (winning_row oplayer) long_rows
      then Just oplayer
      else Nothing

checkValidInput :: String -> State -> IO ()
checkValidInput s state = 
  if s == "s" then 
    saveGame state
  else
    case readMaybe s of
      Just x ->
        if x `elem` validMoves then 
          do
            putStrLn $ "... accept and continue ..."
            game (drop_tile x state)
        else tryAgain s
      Nothing -> tryAgain s
    where
      validMoves = valid_moves state
      tryAgain s =
        do
          putStrLn $ s ++ " is not a valid move, try again: "
          str <- getLine
          checkValidInput str state

saveGame :: State -> IO ()
saveGame state = do
  writeFile "connect4.txt" $ show state
  putStrLn "... game is saved in file connect4.txt and program quits ..."

loadGame :: IO ()
loadGame = do
  file <- readFile "connect4.txt" 
  let state = read file
  game state

restartGame :: Player -> IO ()
restartGame player = 
  do
    putStrLn "Do you want to play another game? (y)es (n)o: "
    str <- getLine
    readRestart str
  where
    loser = if player == 1 then 2 else 1
    readRestart s
      | s == "y" = game $ init_state loser
      | s == "n" = putStrLn "ok bye"
      | otherwise =
        do
          str <- getLine
          readRestart str

{- user interface with I/O -}

main :: IO ()
main = do
  putStrLn "Welcome to Connect Four"
  putStrLn "(n)ew game or (l)oad game: "
  str <- getLine
  if str == "l" then loadGame
  else game $ init_state start_player

game :: State -> IO ()
game state = do
  putStrLn $ show_state state
  case winning_player state of
    Just player ->
      do
        putStrLn $ show_player player ++ " wins!"
        restartGame player
    Nothing -> let moves = valid_moves state in
      if null moves then 
        do
          putStrLn "Game ends in draw."
          restartGame 1
      else do
        putStrLn $ "Choose one of " ++ show moves ++ " or (s)ave: "
        move_str <- getLine
        checkValidInput move_str state

asdf :: Integer -> Integer
asdf a = a

b :: String -> String
b a = a

test :: IO ()
test = 
  do
    putStrLn "HI"
    t <- getLine
    let myTest = asdf $ (read t :: Integer)
    putStrLn $ "HOHO" ++ show myTest
    t2 <- getLine
    let myTest2 = b t2
    putStrLn $ "HOHO" ++ myTest2