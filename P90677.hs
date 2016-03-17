-- A més, només podeu utilitzar recursivitat per definir myFoldl, myFoldr, myIterate, myUntil i myZip.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs
 
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myIterate :: (a -> a) -> a -> [a]
myIterate a b = [b] ++ myIterate a (a b)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f a 
    | c a = a
    | otherwise = myUntil c f (f a)

myMap :: (a -> b) -> [a] -> [b]
myMap f y = [f x | x <- y]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f y = [x | x <- y, f(x)]

myAll :: (a -> Bool) -> [a] -> Bool
myAll f x = and $ map (\x-> f x) x

myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = or $ map (\x-> f x) x

myZip :: [a] -> [b] -> [(a, b)]
myZip a [] = []
myZip [] b = []
myZip (a:as) (b:bs) =  [(a,b)] ++ myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b  = [f x y | (x,y) <- myZip a b]