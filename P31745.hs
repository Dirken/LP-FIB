flatten :: [[Int]] -> [Int]
flatten a = foldl (++) [] a

myLength :: String -> Int
myLength a = foldl (\y x -> y + 1) 0 a

myReverse :: [Int] -> [Int] 
myReverse a = foldr (\x y -> y++[x]) [] a

countIn :: [[Int]] -> Int -> [Int]
countIn x a = map (\l -> length (filter (== a) l)) x

firstWord :: String -> String 
firstWord x = takeWhile (/= ' ') (dropWhile (== ' ') x)
