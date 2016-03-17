--eql :: [Int] -> [Int] -> Bool que indiqui si dues llistes d’enters són iguals.
eql :: [Int] -> [Int] -> Bool
eql x y = (x == y)
--prod :: [Int] -> Int que calculi el producte dels elements d’una llista d’enters.
prod :: [Int] -> Int
prod x = product x

--prodOfEvens :: [Int] -> Int que multiplica tots el nombres parells d’una llista d’enters.
prodOfEvens :: [Int] -> Int
prodOfEvens x = product [y | y <- x, even y]

--powersOf2 :: [Int] que generi la llista de totes les potències de 2.
powersOf2 :: [Int]
powersOf2 = [2^x | x <-[0..]]

scalarProduct :: [Float] -> [Float] -> Float 
scalarProduct x y = sum(zipWith(*) x y)
