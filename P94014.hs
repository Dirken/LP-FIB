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