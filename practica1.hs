{-
----------------------------------------------
| Practica Haskell 2015-2016 Q1 ~kd2ntree~.	 |
| Ricard Meyerhofer i Parra	 				 |
| Llenguatges de Programacio, FIB			 |
----------------------------------------------
-}

-- Exercici 1: Creacio classe point
class Point p where
	sel :: Int -> p -> Double
	dim :: p -> Int
	child :: p -> p -> [Int] -> Int
	dist :: p -> p -> Double
	list2Point :: [Double] -> p

	--Afegides addicionalment:
	point2List :: p -> [Double]
	comp :: p -> p -> Int -> Int
	trans :: [Double] -> p -> p
	scl :: Double -> p -> p 

-- Exercici 2: Impl classe point3D
data Point3d = Point3d (Double,Double,Double) deriving (Eq)
  
instance Show Point3d where
	show (Point3d (p1,p2,p3)) = concat ["(",(show p1),",",(show p2),",",(show p3),")"]

instance Point Point3d where

	--retorna el valor de la coordenada,
	sel i (Point3d (x,y,z)) 
		| i == 1 = x
		| i == 2 = y
		| otherwise = z

	-- dim que donat un element de tipus p retorna la seva dimensio
	dim (Point3d p) = 3

	--retorna el numero del fill de e2 que li toca a e1.
	child e1 e2 a = bin2dec (map (comp e1 e2) a)

	--que es la distancia entre e1 i e2.
	dist (Point3d (x1,y1,z1)) (Point3d (x2,y2,z2)) = sqrt ((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2)

	--list2Point que rep una llista de Double i retorna un element de tipus p.
	list2Point x = (Point3d (x !! 0, x !! 1, x !! 2))

	--point2List rep un punt i el transforma en un []
	point2List x = [sel 1 x, sel 2 x, sel 3 x]

	comp e1 e2 a = if ((sel a e2) > (sel a e1)) then 1 else 0
	
	--Aplica una translacio a un punt
	trans x y = list2Point (zipWith (+) x (point2List y)) 

	--Aplica un escalat a un punt 
	scl x y = list2Point (map (x*) (point2List y))

-- Exercici 3: Def Kd2nTree
data Kd2nTree p = Node p [Int] [Kd2nTree p] 
				| Empty

--Dos arbres son iguals si i nomes si tenen els mateixos elements
instance (Point p, Eq p) => Eq (Kd2nTree p) where
	Empty == Empty = True
	k1 == k2 = (quicksort (getallPoints k1) == (quicksort (getallPoints k2)))

--Escriure arbres
instance Show p => Show (Kd2nTree p) where
	show Empty = ""
	show (Node p1 coords1 fills) = (show p1)++" "++(show coords1)++"\n"++ showF fills 0 0

showF :: Show p => [Kd2nTree p] -> Int -> Int -> String
showF [] s d = ""
showF (Empty:xs) s d = (showF xs (s+1) d)
showF ((Node p2 coords2 fills2):xs) s d = (take (d*4) (cycle " ")) ++ 
                                            " <" ++ (show s) ++ "> "  
                                            ++ (show p2) ++ " " ++ (show coords2) ++ "\n" ++
                                            (showF fills2 0 (d+1)) ++
                                            (showF xs (s+1) d)

-- Exercici 4
--Retorna un Kd2nTree resultant d’inserir el punt al conjunt.
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty p k = Node p k (replicate (2^(length (k))) Empty)
insert (Node p1 coords1 fills) p2 coords2 =  Node p1 coords1 ((take numFill fills) ++
											[insert (fills !! numFill) p2 coords2] ++
											drop (numFill+1) fills)
	where numFill = child p1 p2 coords1


{-- Genera un Kd2nTree de point i una llista de coordenades.
- El Kd2nTree s’ha d’obtenir inserint els elements de la llista en l’ordre 
que apareixen (del primer al darrer).-}
build :: (Eq p, Point p) => [(p, [Int])] -> Kd2nTree p
build x = build2  (reverse x)

build2 :: (Eq p, Point p) => [(p, [Int])] -> Kd2nTree p
build2 [] = Empty
build2 (x:xs) = insert (build2 xs) (fst (x)) (snd (x))

--És com la build pero que rep una llista de parells [([Double],[Int])]
buildIni :: (Eq p, Point p) => [([Double],[Int])] -> Kd2nTree p
buildIni p = build (map (\x -> (list2Point (fst x), (snd x))) p)

--5- Retorna la llista que conte tots els parells amb els punts i les llistes de coordenades del conjunt.
concatenaT [] = []
concatenaT (x:xs) = concat [x, concatenaT xs]

getall :: Point p => Kd2nTree p -> [(p, [Int])]
getall Empty = []
getall (Node punt coord fill) = [(punt, coord)] ++ (concatenaT (map getall fill))

--6 Retorna un Kd2nTree resultant d’eliminar el punt del conjunt
remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty q = Empty 
remove (Node p coords fills) q 
	| p == q = build (concatMap (getall) fills)
	| otherwise = (Node p coords ((take x fills) ++ [remove (fills!!x) p] ++ (drop (x+1) fills)))
        where x = (child p q coords)

--7 Ens diu si el punt pertany o no al conjunt
contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty q = False
contains (Node p coords fills) q = ((p == q) || (contains (fills!!x) q))
	where x = (child p q coords)

--8- (Kd2nTree (no buit)) ens diu quin es el punt mes proper que pertany al conjunt
nearest :: (Point p, Eq p) => Kd2nTree p -> p -> p
nearest (Node p coords fills) q
  | p == q = p
  | otherwise = nearestAux (getall (Node p coords fills)) q p
   
{-9-retorna la llista ordenada amb tots els punts del conjunt que son mes grans o 
iguals que el primer punt i mes petits o iguals que el segon punt.-}
allinInterval :: Point p => Kd2nTree p -> p -> p -> [p]	
allinInterval Empty p q = [] 
allinInterval k p q = quicksort (filter (mesPetitIgual q) (filter ((mesGranIgual p)) (getallPoints k)))

{- Retorna un Kd2nTree de punts de tipus q resultant d’aplicar la 
funcio a tots els punts mantenint l’estructura.-}
kdmap :: (Point p, Point q) => (p -> q)  -> Kd2nTree p -> Kd2nTree q
kdmap f Empty = Empty
kdmap f (Node p1 coords1 fills1) = (Node (f p1) coords1 (map (kdmap f) fills1))

{-Utilitzant el kdmap feu una funcio translation que aplica una translacio 
a tots els punts d’un Kd2nTree (que sera el segon parametre).-}
translation :: (Point p) => [Double] -> Kd2nTree p -> Kd2nTree p
translation	x y = kdmap (trans x) y

--Utilitzant el kdmap feu una funcio scale que aplica una escalat a tots els punts d’un Kd2nTree 
scale :: (Point p) => Double -> Kd2nTree p -> Kd2nTree p
scale x y = kdmap (scl x) y

-- Funcions auxiliars emprades:
bin2dec :: [Int] -> Int
bin2dec [x] = x
bin2dec x = 2*(bin2dec (init x)) + last x

getallPoints :: Point p => Kd2nTree p -> [p]
getallPoints k = (map (fst) (getall k))

nearestAux :: (Point p, Eq p) => [(p,[Int])] -> p -> p -> p
nearestAux [] p q = q
nearestAux ((x,_):xs) p q
    | x == p = nearestAux [] x x
    | (dist x p) < (dist p q) = nearestAux xs p x
    | otherwise = nearestAux xs p q

buit :: (Point p) => Kd2nTree p -> Bool
buit Empty = True
buit (Node p coords fills) = False

mesPetitIgual :: Point p => p -> p -> Bool
mesPetitIgual p q = (sel 1 p >= sel 1 q) && (sel 2 p >= sel 2 q) && (sel 3 p >= sel 3 q)

mesGranIgual :: Point p => p -> p -> Bool
mesGranIgual p q = (sel 1 p <= sel 1 q) && (sel 2 p <= sel 2 q) && (sel 3 p <= sel 3 q)

mesGranIgualL2 :: Point p => p -> p -> Int -> Bool
mesGranIgualL2 p q i
	| (i > (dim p)) || (i > (dim q)) = False
	| (sel i p) >= (sel i q) = True
	| (sel i p) <  (sel i q) = False
	| otherwise = mesGranIgualL2 p q (i+1)

mesPetitL2 :: Point p => p -> p -> Int -> Bool
mesPetitL2 p q i
	| (i > (dim p)) || (i > (dim q)) = False
	| (sel i p) < (sel i q) = True
	| (sel i p) >=  (sel i q) = False
	| otherwise = mesPetitL2 p q (i+1)

mesGranIgualL :: Point p => p -> p -> Bool
mesGranIgualL p q = mesGranIgualL2 p q 1

mesPetitL :: Point p => p -> p -> Bool
mesPetitL p q = mesPetitL2 p q 1

quicksort :: Point p => [p] -> [p]
quicksort []     = []
quicksort (p:xs) = (quicksort (filter (mesPetitL p) xs)) ++ [p] ++ (quicksort (filter (mesGranIgualL p) xs))

exampleSet::Kd2nTree Point3d
exampleSet = buildIni [([3.0, -1.0, 2.1], [1, 3]), ([3.5, 2.8, 3.1], [1, 2]), ([3.5, 0.0, 2.1], [3]), 
    ([3.0, -1.7, 3.1], [1, 2, 3]),([3.0, 5.1, 0.0], [2]), ([1.5, 8.0, 1.5], [1]), ([3.3, 2.8, 2.5], [3]), 
    ([4.0, 5.1, 3.8], [2]),([3.1, 3.8, 4.8], [1, 3]), ([1.8, 1.1, -2.0], [1, 2])]

exampleSet2:: [([Double],[Double])]
exampleSet2 = [([3.0, -1.0, 2.1], [1, 3]), ([3.5, 2.8, 3.1], [1, 2]), ([3.5, 0.0, 2.1], [3]), 
    ([3.0, -1.7, 3.1], [1, 2, 3]),([3.0, 5.1, 0.0], [2]), ([1.5, 8.0, 1.5], [1]), ([3.3, 2.8, 2.5], [3]), 
    ([4.0, 5.1, 3.8], [2]),([3.1, 3.8, 4.8], [1, 3]), ([1.8, 1.1, -2.0], [1, 2])]

exampleSet3::Kd2nTree Point3d
exampleSet3 = buildIni [([2.0, -1.0, 2.1], [1, 3]), ([3.5, 2.8, 3.1], [1, 2]), ([3.5, 0.0, 2.1], [3]), 
    ([3.0, -1.7, 3.1], [1, 2, 3]),([3.0, 5.1, 0.0], [2]), ([1.5, 8.0, 1.5], [1]), ([3.3, 2.8, 2.5], [3]), 
    ([4.0, 5.1, 3.8], [2]),([3.1, 3.8, 4.8], [1, 3]), ([1.8, 1.1, -2.0], [1, 2])]

