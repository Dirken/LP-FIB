
--Donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements de la llista que satisfan el predicat.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf f x = length $ filter f x
--que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes resultant 
--d’aplicar cada una de les funcions de la segona llista als elements de la primera llista. 
--pam [1,2,3] [(+1),(*2),(^2)]
pam :: [Int] -> [Int -> Int] -> [[Int]] 
pam x [] = []
pam x (y:ys) = [(map y x)] ++ (pam x ys)

--que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes 
--on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
pam2 :: [Int] -> [Int -> Int] -> [[Int]] 
pam2 l y = map (\x -> (map ($x) y)) l

--que fa el plegat dels elements que satisfan la propietat donada.
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int 
filterFoldl c f n ll = foldl (f) n [x | x<- ll, c(x)]
-- que donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.

{-
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
--que ordeni la llista per inserció segons la relació donada.
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] -}