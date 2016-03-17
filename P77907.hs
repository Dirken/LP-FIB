absValue :: Int -> Int
absValue x
	| x >= 0 =  x
	| otherwise = -x

power :: Int -> Int -> Int
power x y = x ^ y

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime x =   (0 == length [y | y<-[2..x-1], mod x y == 0])

slowFib :: Int -> Int 
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)

fib2 :: Int-> Int -> Int -> Int
fib2 x y num 
	| num /= 0 = fib2 y (x+y) (num-1)
	| otherwise = x

quickFib :: Int -> Int 
quickFib x = fib2 0 1 x
