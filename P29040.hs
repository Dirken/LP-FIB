insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert tot@(x:xs) y
	| x >= y = y:x:xs
	| maximum tot < y =  tot ++ [y]
	| otherwise = [x] ++ insert xs y

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = (insert (isort xs) x)


remove :: [Int] -> Int -> [Int]
remove [] y = []
remove (x:xs) y 
  | x == y    = xs
  | otherwise = [x] ++ (remove xs y)

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = (minimum l):ssort(remove l (minimum l))
    
--Feu una funció merge :: [Int] -> [Int] -> [Int] que, donades dues 
--llistes ordenades, les fusioni per obtenir una llista amb tots els seus elements ordenats.
merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge tot1@(x:xs) tot2@(y:ys)
    | x > y = y:(merge tot1 ys)
    |otherwise = x:(merge xs tot2)

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort l1) (msort l2)
    where ll = splitAt (div (length l) 2) l 
          l1 = fst ll
          l2 = snd ll

numb :: Int -> [Int] -> ([Int],[Int])
numb _ [] = ([],[])
numb n (y:ys)
	| y < n = (y:l1,l2)
	| otherwise = (l1,y:l2)
		where (l1,l2) = numb n ys
		
qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort(fst (numb x xs)) ++ [x] ++ qsort(snd (numb x xs))


            
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [x] = [x]
genQsort (x:xs) = genQsort(fst (numb x xs)) ++ [x] ++ genQsort(snd (numb x xs))
    where
        numb :: Ord a => a -> [a] -> ([a],[a])
        numb _ [] = ([],[])
        numb n (y:ys)
            | y < n = (y:l1,l2)
            | otherwise = (l1,y:l2)
            where (l1,l2) = numb n ys



