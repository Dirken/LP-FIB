myLength :: [Int] -> Int
myLength x = length x

myMaximum :: [Int] -> Int
myMaximum x = maximum x

average :: [Int] -> Float
average x = (fromIntegral(sum x) / fromIntegral (length (x)))

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = reverse x ++ x

remove2 :: [Int] -> Int -> [Int]
remove2 [] y = []
remove2 (x:xs) y 
  | x == y    = remove2 xs y
  | otherwise = [x] ++ (remove2 xs y)

remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove [] _ = []
remove l (x:xs) = remove (remove2 l x) xs

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

odds :: [Int] -> [Int]
odds x = filter odd x

evens :: [Int] -> [Int]
evens x = filter even x

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens x = (odds x, evens x)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x = (0 == length [y | y<-[2..x-1], mod x y == 0])

primeDivisors :: Int -> [Int]
primeDivisors x = [y | y <-[0..x], isPrime(y) && mod x y == 0]
