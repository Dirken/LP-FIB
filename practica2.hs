import System.Random
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

data TypeOfLine = H | V | DOT deriving (Bounded, Enum, Eq, Show, Read)

instance Random TypeOfLine where
    random g = case randomR (fromEnum (minBound :: TypeOfLine), fromEnum (maxBound :: TypeOfLine)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

data Square = Square TypeOfLine Color | Empty deriving (Eq, Show)

data Board  = Board [[Square]] deriving (Show)
                       
data Match  = Game Board Int Int

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

--Board Functions
createMatch :: Board -> Match
createMatch b = (Game b 0 0)

--creates a board with all the positions empty
initialBoard ::  Int -> Int -> Board 
initialBoard x y = (Board (take x (cycle [take y (cycle [Empty])])))

--when a player wins all the board positions are set to the color of the winner with vertical moves
endGameBoard ::  Board -> Color -> Board 
endGameBoard board@(Board b) color = Board (take (numRows board) (cycle [take (numCols board) (cycle [(Square V color)])]))
  
--modifies the board indicated position with the given parameters 
setSquare :: Int -> Int -> Board -> Square -> Board
setSquare x y (Board b) s = (Board ((take x b) ++ [nSq] ++ (drop (x+1) b))) 
  where nSq = (take y (head $ drop x b)) ++ [s] ++ (drop (y+1) (head $ drop x b))

--returns the board's number of columns                       
numRows :: Board -> Int 
numRows (Board []) = 0
numRows (Board b) = length b

--return the board's number of rows
numCols :: Board -> Int
numCols (Board []) = 0
numCols (Board (b:bs)) = length b

--returns all the empty squares on a board
getAllEmptySquares :: Board -> [(Int,Int)]
getAllEmptySquares (Board b) = boardSearch b 0 

--we break up intro [Square] pieces that represent an entire row that will be solved in searchRow
boardSearch :: [[Square]] -> Int -> [(Int,Int)]
boardSearch b x 
  | (x >= (numRows (Board b))) = []
  | otherwise = (searchRow (b !! x) x 0) ++ (boardSearch b (x+1))

--and for each entire row we look the "Empty" positions
searchRow :: [Square] -> Int -> Int -> [(Int,Int)]
searchRow [] x y = []
searchRow (b:bs) x y 
  | ((mod x 2 == 0) && (b == Empty && (mod y 2 == 0)))  = [(x,y)] ++ searchRow bs x (y+1)
  | ((mod x 2 /= 0) && (b == Empty && (mod y 2 /= 0)))  = [(x,y)] ++ searchRow bs x (y+1)
  | otherwise     = searchRow bs x (y+1)

--It returns if a board is completely full or not
boardFull :: Board -> Bool
boardFull b = (0 == length (getAllEmptySquares b))

--detects is a move done is a winner move
--si tinc x moviments seguits amb la mateix coordenada es win


--Human vs CPU match mode
match1 :: Int -> Board -> IO ()
match1 cpu2 b = do 
  moveHuman (createMatch b) Red cpu2

--CPU vs CPU match mode
match2 :: Int -> Int -> Board -> IO ()
match2 cpu1 cpu2 b = do 
  if (cpu1 == 1) then do
    moveCPU1 (createMatch b) Red cpu1 cpu2
  else if (cpu1 == 2) then do
    moveCPU1 (createMatch b) Red cpu1 cpu2
  else do return()

moveHuman :: Match -> Color -> Int -> IO()
moveHuman (Game b p1 p2) color cpu2 = do
  if (boardFull b) then do 
    winnerMsg 1 b
  else do
    setCursorPosition (2+(numRows b)) (0)
    putStrLn("+ Write the x position selected:")
    x <- readInt;
    putStrLn("+ Write the y position selected:")
    y <- readInt;
    putStrLn ("+ Choose the orientation of the move:")
    putStrLn (" V: Vertical | ")
    putStrLn (" H: Horitzontal _")
    tp <- readLn;
    paintMove (Square tp color) (x,y)
    if (cpu2 == 1) then do
      moveCPUvsHuman1 (Game (setSquare x y b (Square tp color)) p1 p2) Blue cpu2
    else do  moveCPUvsHuman1 (Game (setSquare x y b (Square tp color)) p1 p2) Blue cpu2

moveCPUvsHuman1 :: Match -> Color -> Int -> IO()
moveCPUvsHuman1 (Game b p1 p2) color cpu2 = do
  if (boardFull b) then do 
    winnerMsg 1 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    paintMove (Square tp color) pos
    moveHuman (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu2

moveCPU1 :: Match -> Color -> Int -> Int -> IO()
moveCPU1 (Game b p1 p2) color cpu1 cpu2 = do
  if (boardFull b) then do 
    winnerMsg 1 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    paintMove (Square tp color) pos
    --verticalSearch pos tp && horitzontalSearch pos tp
    if (cpu2 == 1) then do
      moveCPU2 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2
    else do  moveCPU2 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2

moveCPU2 :: Match -> Color -> Int -> Int -> IO()
moveCPU2 (Game b p1 p2) color cpu1 cpu2 = do
  if (boardFull b) then do 
    winnerMsg 2 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    paintMove (Square tp color) pos
    if (cpu1 == 1) then do
      moveCPU1 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2
    else do  moveCPU1 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2


getRandomEmpty :: [a] -> IO a
getRandomEmpty xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

isMoveValid :: Board -> (Int,Int) -> Bool
isMoveValid b (x,y) = (1 == length (filter ((==x). fst)  (filter ((==y). snd) (getAllEmptySquares b))))
--if is equal to one it means that is a valid move because we have that movement available on our board (it's empty)

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

readInt :: IO Int
readInt = readLn

pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 1000000

pause2 :: IO ()
pause2 = do
    hFlush stdout
    -- 10 second pause
    threadDelay 10000000

printDot :: String
printDot = (" " ++ ("."))

printDot2 :: String
printDot2 = ((".") ++ " ") 
  
printBoard :: Int -> Int -> String
printBoard rows cols  = take (2*rows) (cycle (take (2*cols) (cycle printDot)))

printBoard2 :: Int -> Int -> String
printBoard2 rows cols  = take (2*rows) (cycle (take (2*cols) (cycle printDot2)))

printNumsCols :: Int -> IO()
printNumsCols num = do
  setSGR[SetColor Foreground Vivid Yellow]
  putStr (" ") --espai del numero de row
  mapM_ (putStr . show) [0..(2*(num)-1)]
  setSGR[]
  putStrLn ("")

printNumRows :: Int -> IO()
printNumRows num = do
  setSGR[SetColor Foreground Vivid Yellow]
  mapM_ (putStrLn . show) [0..(2*(num)-1)]
  setSGR[]

printDefaultBoard :: Int -> String -> String -> Int -> Int -> IO()
printDefaultBoard iter output output2 s1 s2 = do
  setSGR [SetColor Foreground Vivid Red]
  setCursorPosition s1 (s2)
  putStrLn (output)
  setSGR [SetColor Foreground Vivid Blue]
  setCursorPosition (s1+1) (s2)
  putStrLn (output2)
  setSGR []
  if iter == 1 then do return()
  else do printDefaultBoard (iter-1) output output2 (s1+2) s2


paintMove :: Square -> (Int, Int) -> IO()
paintMove (Square t color) x = do
  setCursorPosition (1+ (fst x)) (1+ (snd x))
  setSGR [SetColor Foreground Vivid color]
  if t == V then do 
    putStr ("|")
  else do 
    putStr ("_")
  setSGR[]


winnerMsg :: Int -> Board-> IO()
winnerMsg winner b = do
  setCursorPosition (2+(numCols b)) (0)
  putStrLn ("The winner is the player" ++ show(winner))

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

main = do
  setTitle "|| Bridgit | Practica Complementaria Haskell | Ricard Meyerhofer Parra | 2015-16Q1 ||"
  clearScreen >> setCursorPosition 0 0
  putStrLn ("Welcome to Bridgit!")
  pause
  clearScreen >> setCursorPosition 0 0
  cursorUpLine 1
  putStr ("+ Introduce the number of rows: ")
  nRowsBoard <- readInt; 
  clearScreen >> setCursorPosition 0 0
  cursorUpLine 1
  putStr ("+ Introduce the number of columns: ")
  nColsBoard <- readInt;
  clearScreen >> setCursorPosition 0 0
  cursorUpLine 1
  putStrLn("+ Choose a game mode:")
  putStrLn("  1: Play against CPU")
  putStrLn("  2: CPU against CPU")
  playOption <- readInt;
  clearScreen >> setCursorPosition 0 0
  if playOption == 1 then do
    putStrLn ("+ Choose the intelligence of your rival:")
    putStrLn (" 1: Low")
    putStrLn (" 2: Medium")
    cpuIAOption <- readInt;
    clearScreen >> setCursorPosition 0 0    
    
    printNumsCols nColsBoard
    printNumRows nRowsBoard
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard) (printBoard2 nColsBoard nRowsBoard) 1 1
    match1 cpuIAOption (initialBoard ((2*nRowsBoard)-1) ((2*nColsBoard)-1))
    
    pause2
    clearScreen >> setCursorPosition 0 0
    putStrLn ("+ Do you want to play again?")
    putStrLn (" 1: Yes")
    putStrLn (" 2: No")
    rematch <- readInt;
    if rematch == 1 then do main
    else do return()
  else do
    putStrLn ("+ Choose the intelligence of the first player:")
    putStrLn (" 1: Low")
    putStrLn (" 2: Medium")
    cpuIAOption1 <- readInt;
    clearScreen >> setCursorPosition 0 0
    putStrLn ("+ Choose the intelligence of the second player:")
    putStrLn (" 1: Low")
    putStrLn (" 2: Medium")
    cpuIAOption2 <- readInt;
    clearScreen >> setCursorPosition 0 0

    printNumsCols nColsBoard
    printNumRows nRowsBoard
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard) (printBoard2 nColsBoard nRowsBoard) 1 1
    match2 cpuIAOption1 cpuIAOption2 (initialBoard ((2*nRowsBoard)) ((2*nColsBoard)))

    pause2
    clearScreen >> setCursorPosition 0 0
    putStrLn ("+ Do you want to play again?")
    putStrLn (" 1: Yes")
    putStrLn (" 2: No")
    rematch2 <- readInt;
    if rematch2 == 1 then do main
    else do return()
  return ()
