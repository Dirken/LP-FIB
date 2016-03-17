--Accions com ara [1..], perÃ² Ã©s recomanable utilitzar funcions dâ€™ordre superior com ara map, scanl, iterate, filter, ...
--[1,1,1,1,1,1,1,1,â€¦].
ones :: [Integer] 
ones = [1] ++ ones
--ones = repeat[1] o tambÃ© usant cycle

--[0,1,2,3,4,5,6,7â€¦].
nats :: [Integer] 
nats = (iterate(+1) 0)

--[0,1,âˆ’1,2,âˆ’2,3,âˆ’3,4â€¦].
ne 0 = 1
ne x
  |x < 0 = (-1 * x) + 1
  |otherwise = (-1 * x)
  
ints :: [Int]
ints = iterate ne 0

--nombres triangulars: 0,1,3,6,10,15,21,28,â€¦].
triangulars :: [Integer] 
triangulars = 0 : next 0 1 where next t v = (t+v) : (next (t+v) (v+1))
--nombres factorials: [1,1,2,6,24,120,720,5040,â€¦].
fact1 :: Integer -> Integer
fact1 n
    | n == 0 = 1
    | otherwise = n*(fact1 (n-1))
factorials :: [Integer] 
factorials =  (map fact1 nats)

--nombres de Fibonacci: [0,1,1,2,3,5,8,13,â€¦].
-- (Private) fib n = (F(n), F(n+1))
fibonacci :: Integer -> (Integer, Integer)
fibonacci 0 = (0, 1)
fibonacci n =
  let (a,b) = fibonacci (div n 2)
      c = a * (b * 2 - a)
      d = a * a + b * b
  in if mod n 2 == 0
    then (c,d)
    else (d, c + d)

fib :: Integer -> Integer
fib n | n >= 0 = fst (fibonacci n)
fibs::[Integer]
fibs = (map fib nats)
{-fibs::[Integer]
fibs = 0: scanl (+) 1 fibs-}

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime v = let maxDiv = floor $ sqrt $ fromIntegral v
            in all ((/= 0).(v `rem`)) $ takeWhile (<= maxDiv) primes

nats1 :: Integer -> [Integer]
nats1 x = x : nats1 (x+1)

primes :: [Integer]
primes = filter isPrime (nats1 0)

--nombres de Hamming: [1,2,3,4,5,6,8,9,â€¦]. Els nombres de Hamming sÃ³n aquells que nomÃ©s tenen 2, 3 i 5 com a divisors primers.
hammings :: [Integer]
hammings = 1 : weave hammings hammings hammings
  where
    weave (x2:x2s) (x3:x3s) (x5:x5s) = minX : weave x2s' x3s' x5s'
      where
        (x2',x3',x5') = (2*x2,3*x3,5*x5)
        minX = minimum [x2', x3', x5']
        x2s' = if x2' <= minX then x2s else x2:x2s
        x3s' = if x3' <= minX then x3s else x3:x3s
        x5s' = if x5' <= minX then x5s else x5:x5s
-- noDivs x y es true si x no es divisible x cap nombre dsd y fins a 2
noDivs :: Integer -> Integer -> Bool
noDivs x 1 = True
noDivs x y =
  if mod x y == 0 then False
  else noDivs x (y - 1)
       
divsList :: Integer -> Integer -> [Integer]
divsList x 1 = []
divsList x y =
  if (mod x y == 0) && isPrime y then 
    l ++ [y]
  else l
  where l = divsList x (y - 1)
        
-- retorna la llista de divisors primers de lentrada
primeDivisors :: Integer -> [Integer]
primeDivisors 1 = []
primeDivisors x
    | isPrime x = [x]
    | otherwise = divsList x (x-1)
 
--mira i digues: [1,11,21,1211,111221,312211,13112221,1113213211,â€¦].
group' :: (Eq a) => [a] -> [[a]]
group' []     = []
group' (x:xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

lookNsay :: [Integer]
lookNsay = 1 : map awesomeFunction lookNsay

awesomeFunction :: Integer -> Integer
awesomeFunction alist =
  (read (foldr (\x xs -> (show . length) x ++ head x : xs) [] $ group' (show alist))) :: Integer

--triangle de Tartaglia (tambÃ© anomenat triangle de Pascal): [[1],[1,1],[1,2,1],[1,3,3,1],â€¦]. 
ntart :: [Integer] -> [Integer]
ntart [] = []
ntart [x] = []
ntart l =  [x] ++ l2
          where x = head l + (head $ drop 1 l)
                l2 = ntart $ drop 1 l

ntart1 :: [Integer] -> [Integer]
ntart1 l = [1] ++ (ntart l) ++ [1]

tartaglia :: [[Integer]]
tartaglia = iterate ntart1 [1]
