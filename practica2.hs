import System.Random
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad


---------------Board-----------------

data TypeOfLine = H | V deriving (Bounded, Enum, Eq, Show)

instance Random TypeOfLine where
    random g = case randomR (fromEnum (minBound :: TypeOfLine), fromEnum (maxBound :: TypeOfLine)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

data Square = Square (Int,Int) TypeOfLine Color | Empty deriving (Eq, Show)

data Board  = Board [[Square]] deriving (Show)
                       
data Match  = Game Board Int Int

--board number of cols                             
numCols :: Board -> Int 
numCols (Board []) = 0
numCols (Board b) = length b

--board number of rows
numRows :: Board -> Int
numRows (Board []) = 0
numRows (Board (b:bs)) = length b

{-
isMovimentValid :: Board -> (Int, Int) -> Bool
isMovimentValid b (x,y) 
  | (any (x,y)||(y,x) getAllMovements) && makeSense = False
  | otherwise = True

makeSense :: Board -> (Int, Int) -> Bool
makeSense b coord 
  | = False
  | otherwise = True

getAllEmptySquares :: Board -> [(Int,Int)]
getAllEmptySquares =

getAllMovements ::

sortbyCol :: 

sortByRow :: 

winnerMove :: [(Int, Int)] -> Bool

match1 :: Int -> Int -> Int -> IO()
match1 dif nc nr = do
  tiradaHum (creaPartida nc nr) 1

match2 :: Int -> Int -> Int -> Int -> IO ()
partida2 dif1 dif2 nc nr = do 
  tirada (creaPartida nc nr) 1
-}

-- Retorna una posicio buida al tauler (escollida aleatoriament)
{-}
getRandomEmpty :: Board -> (Int, Int) 
getRandomEmpty b = do
    rand <- randomRIO (0, (length allEmptySquares -1))
    return (allEmptySquares !! rand)
  where allEmptySquares = getEmptySquares b
  -}    

randomType :: RandomGen g => g -> (TypeOfLine, g)
randomType g = case randomR (0,1) g of
                   (r, g') -> (toEnum r, g')

getRandomType :: TypeOfLine
getRandomType =  fst (randomType newStdGen)
-----------------------------------
---------------IO---------------------
--------------------------------------
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
  
paintMove :: Square -> IO()
paintMove (Square x t color) = do
  setCursorPosition (1+ fst x) (1+ snd x)
  setSGR [SetColor Foreground Vivid color]
  if t == V then do 
    putStr ("|")
  else do 
    putStr ("_")
  setSGR[]

winnerMsg :: Int -> IO()
winnerMsg winner = do
  putStrLn ("The winner is the player" ++ show(winner))

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
    --
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard) (printBoard2 nColsBoard nRowsBoard) 1 1
    --match1  cpuIAOption nRowsBoard nColsBoard
    --
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
    --
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard) (printBoard2 nColsBoard nRowsBoard) 1 1
    --paintMove (Square (1,5) V Blue)
    --match2 cpuIAOption1 cpuIAOption2 nRowsBoard nColsBoard
    --
    pause2
    clearScreen >> setCursorPosition 0 0
    putStrLn ("+ Do you want to play again?")
    putStrLn (" 1: Yes")
    putStrLn (" 2: No")
    rematch2 <- readInt;
    if rematch2 == 1 then do main
    else do return()
  return ()
