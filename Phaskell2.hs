import System.Random
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

data TypeOfLine = H | V | Blank deriving (Bounded, Enum, Eq, Show, Read)

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

--Human vs CPU match mode
match1 :: Int -> Board -> Int -> Int -> Int -> IO ()
match1 cpu2 b option punt1 punt2 = do 
	if option == 1 then do 
 	  moveHuman (createMatch b) Red cpu2 option punt1 punt2
 	else do
 	  moveCPUvsHuman1 (createMatch b) Blue cpu2 option punt1 punt2

--CPU vs CPU match mode
match2 :: Int -> Int -> Board -> Int -> Int -> IO ()
match2 cpu1 cpu2 b punt1 punt2= do 
  if (cpu1 == 1) then do     moveCPU1 (createMatch b) Red cpu1 cpu2 punt1 punt2
  else if (cpu1 == 2) then do
    moveCPU1S (createMatch b) Red cpu1 cpu2 punt1 punt2
  else do return()

--ens retorna el tipus d'un Square concret
getSquareType :: Square -> TypeOfLine
getSquareType (Square t c)
	| t == H = H
	| t == V = V
	| otherwise = Blank
getSquareType Empty = Blank

compleix1V :: [[Square]] -> Int -> Int -> Bool
compleix1V b x y = ((getSquareType(b !! (x+2) !! y) == V) &&  (getSquareType(b !! (x+1) !! (y+1)) == H) &&
					(getSquareType(b !! (x-1) !! (y+1)) == H)) || ((getSquareType(b !! (x-1) !! (y-1)) == H) &&  
					(getSquareType(b !! (x) !! (y-2)) == V) && (getSquareType(b !! (x+1) !! (y-1)) == H))

compleix2V :: [[Square]] -> Int -> Int  -> Bool
compleix2V b x y = ((getSquareType(b !! (x+2) !! y) == V) &&  (getSquareType(b !! (x+1) !! (y+1)) == H) &&
					(getSquareType(b !! (x-1) !! (y+1)) == H)) && ((getSquareType(b !! (x-1) !! (y-1)) == H) &&  
					(getSquareType(b !! (x) !! (y-2)) == V) && (getSquareType(b !! (x+1) !! (y-1)) == H))

compleix1H :: [[Square]] -> Int -> Int -> Bool
compleix1H b x y = ((getSquareType(b !! (x-1) !! (y+1)) == V) && (getSquareType(b !! (x-1) !! (y-1)) == V) &&
				   (getSquareType(b !! (x-2) !! y) == H)) || ((getSquareType(b !! (x+1) !! (y-1)) == V) &&
				   (getSquareType(b !! (x+1) !! (y+1)) == V) && (getSquareType(b !! (x+2) !! (y)) == H))

compleix2H :: [[Square]] -> Int -> Int  -> Bool
compleix2H b x y = (getSquareType(b !! (x-1) !! (y+1)) == V) && (getSquareType(b !! (x-1) !! (y-1)) == V) &&
				   (getSquareType(b !! (x-2) !! y) == H) &&  (getSquareType(b !! (x+1) !! (y-1)) == V) &&
				   (getSquareType(b !! (x+1) !! (y+1)) == V) && (getSquareType(b !! (x+2) !! (y)) == H)

--determines if the movement done give the player points
scoreMove1 :: Board -> (Int, Int) -> Int -> Int -> TypeOfLine -> IO()
scoreMove1 all@(Board b) coord punt1 punt2 tp 
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix2H b (fst coord) (snd coord)) && tp 	== H  = printScores (punt1+2) punt2 all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix1H  b (fst coord) (snd coord)) && tp == H  = printScores (punt1+1) punt2 all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix2V b (fst coord) (snd coord)) && tp 	== V  = printScores (punt1+2) punt2 all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix1V  b (fst coord) (snd coord)) && tp == V  = printScores (punt1+1) punt2 all
	| otherwise =  printScores punt1 punt2 all 

--determines if the movement done give the player points
scoreMove2 :: Board -> (Int, Int) -> Int -> Int -> TypeOfLine -> IO()
scoreMove2 all@(Board b) coord punt1 punt2 tp 
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix2H b (fst coord) (snd coord)) && tp == H  = printScores punt1 (punt2+2) all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix1H b (fst coord) (snd coord)) && tp == H  = printScores punt1 (punt2+1) all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix2V b (fst coord) (snd coord)) && tp == V  = printScores punt1 (punt2+2) all
	| (fst coord >= 2) && (snd coord >= 2) &&(compleix2V b (fst coord) (snd coord)) && tp == V  = printScores punt1 (punt2+1) all
	| otherwise =  printScores punt1 punt2 all

moveHuman :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveHuman (Game b p1 p2) color cpu2 option punt1 punt2 = do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
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
    if (option == 1) then do scoreMove1 b (x,y) punt1 punt2 tp --wrong use of option
    else do scoreMove2 b (x,y) punt1 punt2 tp
    if (cpu2 == 1) then do
      moveCPUvsHuman1 (Game (setSquare x y b (Square tp color)) p1 p2) Blue cpu2 option punt1 punt2
    else do  moveCPUvsHuman1 (Game (setSquare x y b (Square tp color)) p1 p2) Blue cpu2 option punt1 punt2

moveCPUvsHuman1 :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveCPUvsHuman1 (Game b p1 p2) color cpu2 option punt1 punt2= do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    paintMove (Square tp color) pos
    if (option == 1) then do scoreMove1 b pos punt1 punt2 tp
    else do scoreMove2 b pos punt1 punt2 tp

    moveHuman (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu2 option punt1 punt2

--Els hi falta lo de la opcio i tal.
--Dummy
moveCPU1 :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveCPU1 (Game b p1 p2) color cpu1 cpu2  punt1 punt2= do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    --scoreMove1 b pos punt1 punt2  tp
    paintMove (Square tp color) pos
    --verticalSearch pos tp && horitzontalSearch pos tp
    if (cpu2 == 1) then do
      moveCPU2 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2 punt1 punt2
    else do  moveCPU2S (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2 punt1 punt2

--"Smart" IA
moveCPU1S :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveCPU1S (Game b p1 p2) color cpu1 cpu2  punt1 punt2 = do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
  else do
    let pos = (getBestOption b)
    let tp  = (getBestType b pos)
    paintMove (Square tp color) pos
    --scoreMove1 b pos punt1 punt2 tp
    --verticalSearch pos tp && horitzontalSearch pos tp
    if (cpu2 == 1) then do
      moveCPU2 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2 punt1 punt2
    else do  moveCPU2S (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Blue cpu1 cpu2 punt1 punt2

--Dummy
moveCPU2 :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveCPU2 (Game b p1 p2) color cpu1 cpu2 punt1 punt2 = do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
  else do
    pos <-(getRandomEmpty (getAllEmptySquares b))
    tp <- randomRIO(H,V)
    paintMove (Square tp color) pos
    --scoreMove1 b pos punt1 punt2 tp
    --printScores cpu1 cpu2 b
    if (cpu1 == 1) then do
      moveCPU1 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2 punt1 punt2
    else do  moveCPU1S (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2 punt1 punt2

--"Smart" IA
moveCPU2S :: Match -> Color -> Int -> Int -> Int -> Int -> IO()
moveCPU2S (Game b p1 p2) color cpu1 cpu2 punt1 punt2 = do
  if (boardFull b) then do 
    winnerMsg punt1 punt2 b
  else do
    let pos = (getBestOption b)
    let tp  = (getBestType b pos)
    paintMove (Square tp color) pos
    --scoreMove2 b pos punt1 punt2 tp
    if (cpu1 == 1) then do
      moveCPU1 (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2 punt1 punt2
    else do  moveCPU1S (Game (setSquare (fst pos) (snd pos) b (Square tp color)) p1 p2) Red cpu1 cpu2 punt1 punt2


--IA that simply does the following:
-- If we have a chance of score, we score (we do this just by using compleix functions).
-- Otherwise we just simply move since a better IA can get really hard to archive and also because 
-- i don't see any other way to play that does not make you lose (if we want to go creating squares) someone 
-- that has a bit of intelligence will simply get what are you doing and will do exactly what this IA pretends
-- to do and in the worst case it will get a tie.

--compleix1V :: [[Square]] -> Int -> Int -> Bool
bestOption :: [[Square]] -> [Int] -> [Int] -> (Int,Int)
bestOption b [] [] = (-1,-1)
bestOption b (x:xs) (y:ys) 
	| (compleix2V b x y 	|| compleix2H b x y) 	= (x,y)   
	| otherwise 			=  bestOption b xs ys

bestOption2 :: [[Square]] -> [Int] -> [Int] -> (Int,Int)
bestOption2 b [] [] = (-1,-1)
bestOption2 b (x:xs) (y:ys) 
	| (compleix1V b x y 	|| compleix1H b x y) 	= (x,y)
	| otherwise 			=  bestOption2 b xs ys

getBestOption :: Board -> (Int,Int)
getBestOption all@(Board b) 
	| (-1,-1) /= bestOption b (map fst  (getAllEmptySquares all)) (map snd (getAllEmptySquares all)) = bestOption b (map fst  (getAllEmptySquares all)) (map snd (getAllEmptySquares all))
	| (-1,-1) /= bestOption2 b (map fst  (getAllEmptySquares all)) (map snd (getAllEmptySquares all)) = bestOption2 b (map fst  (getAllEmptySquares all)) (map snd (getAllEmptySquares all))
	| otherwise = x
		where (x:xs) = (getAllEmptySquares all) 

getBestType :: Board -> (Int, Int) -> TypeOfLine
getBestType (Board b) (x,y) 
	| compleix1V b x y || compleix2V b x y = V
	| otherwise = H

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
  
printBoard :: Int -> Int -> String
printBoard rows cols  = take (2*rows) (cycle (take (2*cols) (cycle printDot)))

printNumsCols :: Int -> IO()
printNumsCols num = do
  setSGR[SetColor Foreground Vivid Yellow]
  putStr (" ") --espai del numero de row
  mapM_ (putStr . show) [0..((2*num)-1)]
  setSGR[]
  putStrLn ("")

printNumRows :: Int -> IO()
printNumRows num = do
  setSGR[SetColor Foreground Vivid Yellow]
  mapM_ (putStrLn . show) [0..((2*num)-1)]
  setSGR[]

printDefaultBoard :: Int -> String -> Int -> Int -> IO()
printDefaultBoard iter output s1 s2 = do
  setSGR [SetColor Foreground Vivid Red]
  setCursorPosition s1 (s2)
  putStrLn (output)
  setSGR []
  if iter == 1 then do return()
  else do printDefaultBoard (iter-1) output (s1+2) s2

printScores :: Int -> Int -> Board -> IO()
printScores p1 p2 b = do
	setCursorPosition (10+(numRows b)) (0)
	setSGR[SetColor Foreground Vivid Yellow]
	putStrLn ("Score:")
	putStrLn ("Player1 " ++ show(p1))
	putStrLn ("Player2 " ++ show(p2))
	setSGR []

paintMove :: Square -> (Int, Int) -> IO()
paintMove (Square t color) x = do
  setCursorPosition (1+ (fst x)) (1+ (snd x))
  setSGR [SetColor Foreground Vivid color]
  if t == V then do 
    putStr ("|")
  else do 
    putStr ("_")
  setSGR[]

--falta modificar que sigui el que tingui més punts.
winnerMsg :: Int -> Int -> Board-> IO()
winnerMsg p1 p2 b = do 

  if p1 > p2 then do
  	setCursorPosition (8+(numRows b)) (0)   
  	putStrLn ("The winner is the Player1")
  	else do return()
  if p2 > p1 then do   
  	setCursorPosition (8+(numRows b)) (0)
  	putStrLn ("The winner is the Player2")
  else do 
  	setCursorPosition (8+(numRows b)) (0)
  	putStrLn ("DRAW")

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

welcome = do
  setTitle "|| Squares | Practica Complementaria Haskell | Ricard Meyerhofer Parra | 2015-16Q2 ||"
  clearScreen >> setCursorPosition 0 0
  putStrLn ("Welcome to Squares!")
  pause
  clearScreen >> setCursorPosition 0 0
  cursorUpLine 1
  return()


main = do
  welcome
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

    putStrLn ("+ Do you want to play first?:")
    putStrLn (" 1: Yes")
    putStrLn (" 2: No")
    whoFirst <- readInt;
    clearScreen >> setCursorPosition 0 0
    
    printNumsCols nColsBoard
    printNumRows nRowsBoard
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard)  1 1
    match1 cpuIAOption (initialBoard ((2*nRowsBoard)-1) ((2*nColsBoard)-1)) whoFirst 0 0
    
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
    printDefaultBoard nRowsBoard (printBoard nRowsBoard nColsBoard) 1 1
    match2 cpuIAOption1 cpuIAOption2 (initialBoard ((2*nRowsBoard)) ((2*nColsBoard))) 0 0

    pause2
    clearScreen >> setCursorPosition 0 0
    putStrLn ("+ Do you want to play again?")
    putStrLn (" 1: Yes")
    putStrLn (" 2: No")
    rematch2 <- readInt;
    if rematch2 == 1 then do main
    else do return()
  return ()


{-I've reused great part of my Bridgit game code -}