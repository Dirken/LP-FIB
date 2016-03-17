--Donada una llista d’enters, calculi la seva llargada.
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = myLength(xs) + 1