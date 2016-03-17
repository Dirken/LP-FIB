multiple35 :: Int -> Either Int String
multiple35 n 
	| mod n 3 == 0 && mod n 5 == 0 = Right  "FizzBuzz"
	| mod n 3 == 0 && mod n 5 /= 0 = Right  "Fizz"
	| mod n 3 /= 0 && mod n 5 == 0 = Right  "Buzz"
	| otherwise = Left n
fizzBuzz :: [Either Int String]
fizzBuzz = map multiple35 [0..]