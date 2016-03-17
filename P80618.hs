{-
cua = Queue [2,8,5] [4,7]
que representa la cua on el primer és el 2 i segueix amb 8, 5, 7 i 4.

D’aquesta manera, l’operació d’afegir en una cua es fa posant el nou element per 
davant de la segona llista (que és menys costós que afegir-lo al final d’una llista).

D’altra banda, l’operació d’avançar es fa treient el primer de la primera llista, si en té,
i sinó, passant els de la segona llista cap a la primera (en l’ordre correcte) i agafant el
primer tot deixant la resta.
-}



data Queue a = Queue [a][a]
             deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue a b) = (Queue a (x:b))
        
pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (drop 1 l1) [])
  where l1 = myReverse l2
pop (Queue l1 l2) = (Queue (drop 1 l1) l2)

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue l1 l2) = False

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue (x:l1) l2) = x
           

instance (Eq a) => Eq (Queue a) where
  Queue [][] == Queue [][] = True
  q1 == q2  
    | (empty q1 || empty q2) = False
    | otherwise = if (top q1) == (top q2) 
             then ((pop q1) == (pop q2)) 
             else False

myReverse :: [a] -> [a]
myReverse = foldl (\l x -> ([x] ++ l)) []
        
sizeQ :: Queue a -> Int
sizeQ (Queue l1 l2) = (length l1) +  (length l2)