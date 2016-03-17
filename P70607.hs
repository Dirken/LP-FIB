--que, donada una llista d’enters, calculi la seva llargada utilitzant recursivitat.
myLength1 :: [Int] -> Int 
myLength1 [] = 0
myLength1 (x:xs) = 1 + myLength1 xs
--que, donada una llista d’enters, calculi la seva llargada sense utilitzar recursivitat.
myLength2 :: [Int] -> Int 
myLength2 x = sum (map (\_ -> 1) x)
