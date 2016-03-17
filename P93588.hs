--Emuli el map usant llistes per comprensió.
myMap :: (a -> b) -> [a] -> [b]
myMap f b = [f x| x <- b]
-- Emuli el filter usant llistes per comprensió.
myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter y z = [x | x <- z, y x]
-- Emuli el zipWith usant llistes per comprensió i zip.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f b c = [f x y | (x,y) <- (zip b c)]
--Donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify a b = [(x,y)| x <- a, y <- b, mod x y == 0]
--Donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).
factors :: Int -> [Int]
factors y = [x | x <- [1..y], mod y x == 0 ]
