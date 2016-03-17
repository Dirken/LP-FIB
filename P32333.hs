{-Muchas otras maneras de resolverlo en:
http://www.willamette.edu/~fruehr/haskell/evolution.html
-}
fact1 :: Integer -> Integer
fact1 n
	| n == 0 = 1
	| otherwise = n*(fact1 (n-1))

fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n = n*(fact2 (n-1))

fact3 :: Integer -> Integer
fact3 n = 
	if n == 0 then 1
	else n*fact3(n-1)

fact4 :: Integer -> Integer
prod :: [Integer] -> Integer
prod [x] = x
prod (x:xs) = x * prod(xs)
fact4 x = prod[1..x]

fact5 :: Integer -> Integer
fact5 x = foldl (*) x [1..(x-1)]

fact6 :: Integer -> Integer
facs = scanl (*) 1 [1..]
fact6 n = facs !! fromInteger n

fact7 :: Integer -> Integer
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fact7 = facAcc 1

fact8 :: Integer -> Integer
fact8 n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m

for i n d = until d n i