{-
-------------------------------------------------------------
| Practica Haskell 2015-2016 Q2 Programes imperatius.	 	|
| Ricard Meyerhofer i Parra	 				 				|
| Llenguatges de Programacio, FIB			 				|
-------------------------------------------------------------
-}

-------------------------------------------------------------
--2															|
-------------------------------------------------------------

type Ident = String

data ExpNum a =  Const a | Var Ident | Plus (ExpNum a) (ExpNum a) | Minus (ExpNum a) (ExpNum a) 
				| Div (ExpNum a) (ExpNum a) | Times (ExpNum a) (ExpNum a)

data ExpBool a = Gt (ExpNum a) (ExpNum a) | Eq (ExpNum a) (ExpNum a) | AND (ExpBool a) (ExpBool a) 
				| OR (ExpBool a) (ExpBool a) | NOT (ExpBool a)

data Command a = Assign Ident (ExpNum a) | Input Ident | Print (ExpNum a) | Seq [Command a] 
                | Cond (ExpBool a) (Command a) (Command a) | Loop (ExpBool a) (Command a) 

--Mostri el codi identat adequadament

instance Show a => Show (Command a) where
	show a = (showC a 0)

ident :: Int -> String
ident x = (take (x*2) (cycle " "))

showC :: Show a => Command a -> Int -> String
showC (Input a) depth	 = ident depth  ++ "INPUT "  ++ (show a) ++ ";\n"
showC (Assign a b) depth = ident depth ++ a ++ " := "  ++ (show b) ++ ";\n"
showC (Print a) depth	 = ident depth ++ "PRINT "  ++ (show a) ++ ";\n"
showC (Seq []) depth 	 = ident depth ++ ""
showC (Seq a) depth 	 = concatMap (\x -> showC x depth) a
showC (Cond a b c) depth = ident depth ++ "IF " ++ (show a) ++ " THEN\n" ++ (showC b (depth+1)) 
							++ ident depth ++ "ELSE\n"++ (showC c (depth+1)) ++ ident depth ++"END\n"
showC (Loop a b) depth 	 = ident depth ++ "WHILE " ++ (show a) ++ 
							"\n" ++ ident depth ++"DO\n" ++ (showC b (depth+1)) ++ ident depth ++ "END\n"

instance Show a => Show (ExpNum a) where
	show (Const a) 		= (show a)
	show (Var a) 		= a
	show (Plus a b )	= (show a) ++ " + " ++ (show b)
	show (Minus a b)	= (show a) ++ " - " ++ (show b)
	show (Div a b)		= (show a) ++ " / " ++ (show b)
	show (Times a b)	= (show a) ++ " * " ++ (show b)

instance Show a => Show (ExpBool a) where
	show (NOT a) 	= "NOT " ++ (show a)
	show (AND a b)  = (show a) ++ " AND " ++ (show b)
	show (OR a b) 	= (show a) ++ " OR " ++ (show b)
	show (Eq a b)	= (show a) ++ " = "  ++ (show b)
	show (Gt a b)	= (show a) ++ " > "	 ++ (show b)

{-Considerem que dos programes son iguals si les seves versions expandides
i simplificades son estructuralment isomorfes-}
instance (Eq a) => Eq (Command a) where
	a1 == a2 = isomorf (expand $ simplify a1) (expand $ simplify a2)

{-
-------------------------------------------------------------
ReadCommand: No m'ha donat temps d'acabar el Read i 		|
 per tant el comento sencer.								|
No he acabat de poder fer la detecció de blocs i els parsers|
que he fet no els he pogut acabar de mirar de compilar etc	|
es com una mica de pseudocodi tot i que és haskell. 		|
Ho deixo per si es valora l'idea o l'intent del que 		|
pugui tenir													|
-------------------------------------------------------------

putComma :: String -> String 
putComma ("END")	= "END;"
putComma ("THEN")	= "THEN;"
putComma ("ELSE")	= "ELSE;"
putComma ("DO")		= "DO;"
putComma (x)		= x
	
readCommand :: Read a => String -> Command a
readCommand entrada = createCommands $ unwords $  map (putComma) $ words entrada

filtra :: String -> String
filtra entrada = (takeWhile (/=';')) entrada

createCommands :: String -> Command a
createCommands []					= Seq[]
createCommands (("INPUT"):xs:xss) 	= Seq $ (Input (filtra $ xs)) : [createCommands xss]
createCommands (("PRINT"):xs:xss)	= Seq $ (Print ((Var $ filtra $ xs))) : [createCommands xss]
createCommands (("IF"):xs)			= Seq $ (Cond extractCB(xs) extractBody(xs) extractElse(xs))
createCommands (("WHILE"):xs)		= Seq $ (Loop extractCB(xs) extractBody(xs))
createCommands (_:xs:xss:xsss)		= Seq $ (Assign xs xss): [createCommands xsss]

extractBody :: [String] -> Command a

extractElse :: [String] -> Command a

extractCB :: [String] -> ExpBool a
extractCB [] = []
extractCB x = converteixCB $ takeWhile (/= "THEN") x 

getInstruction  :: [String] -> ([String],[String])
getInstruction x = span (/=";") prog

converteixCB :: Read a => [String] -> ExpBool a
converteixCB [] 			= []
converteixCB ("NOT":a) 		= (NOT (converteixCB a))
converteixCB x 
    | r == "OR" 			= OR parsedCB  (converteixCB rs)
    | r == "AND" 			= AND parsedCB (converteixCB rs)
    | r2 == ">" 			= Gt parsedNE  (converteixCB rs2)
    | r2 == "=" 			= Eq parsedNE  (converteixCB rs2)
		where (l, (r:rs)) 	= span $ notIN "OR" "AND" x 
			  (l2, (r2:rs2))= span $ notIN "Gt" "Eq" x
			  parsedCB 		= converteixCB l
			  parsedNE 		= converteixNE  l2

converteixNE :: Read a => [String] -> ExpNum a
converteixNE (x:[]) = Var x
converteixNE x
	| r == "+" = Plus parsedLeftNumExp  (parseNumExp rs)
    | r == "-" = Minus parsedLeftNumExp  (parseNumExp rs)
    | r == "*" = Times parsedLeftNumExp (parseNumExp rs2)
    | r == "/" = Div parsedLeftNumExp  (parseNumExp rs2)
	    where	(l, (r:rs))    	= span notIN "+" "-" x
	        	(l2, (r2:rs2)) 	= span notIn "*" "/" xs
	        	parsedNE 		= converteixNE l
	        	parsedNE2 		= converteixNE l

converteixNE (all@(digit:xs):[])
    | '0' <= digit && digit <= '9' = Const $ read all

notIN :: String -> String  -> [String] -> Bool
notIN a b x = (x/=a) && (x/=b)
-}

-------------------------------------------------------------
--3															|
-------------------------------------------------------------
class SymTable m where
	update :: m a -> String -> a -> m a
	value  :: m a -> String -> a
	exists :: m a -> String -> Bool
	start  :: m a

data Memoria1 a = M [(String, a)] deriving (Show)

instance SymTable Memoria1 where

	update (M []) p q 	= M ((p,q):[])
  	update (M ((x,y):xs)) p q
	    | (x == p ) 	= M ((p,q):xs)
	    | otherwise 	= M ((x,y):aux)
	    	where M aux = (update (M xs) p q)

	value (M ((x,y):xs)) s
		| (x == s) 		= y
		| otherwise 	= value (M xs) s

	exists (M []) s	  	= False
	exists (M ((x,y):xs)) s
		| (x == s) 		= True
		| otherwise 	= exists (M xs) s

	start = M []

--ordenat per la primera component
data Memoria2 a =  Node String a (Memoria2 a) (Memoria2 a) | Empty deriving (Show)

instance SymTable Memoria2 where

	update Empty p q 		= Node p q Empty Empty
	update (Node p s f1 f2) a b
		| (p == a)			= Node p b f1 f2
		| (p < a)			= Node p s f1 (update f2 a b)
		| otherwise 		= Node p s (update f1 a b) f2

	value (Node p s f1 f2) a
		| p == a 			= s
		| p < a 			= value f1 a
		| otherwise 		= value f2 a

	exists (Empty) s 		= False
	exists (Node p s f1 f2) a
		| p == a 			= True
		| p < a 			= exists f1 a
		| otherwise 		= exists f2 a

	start = Empty

--evaluaCondicionsBooleanes
evaluaCB :: (SymTable m, Num a, Ord a) => m a -> ExpBool a -> (Either Bool String)
evaluaCB m (Gt x y) 	= genError (evaluaNum  m x) (>) (evaluaNum  m y)
evaluaCB m (Eq x y) 	= genError (evaluaNum  m x) (==) (evaluaNum  m y)
evaluaCB m (AND x y)	= genError (evaluaCB  m x) (&&) (evaluaCB  m y)
evaluaCB m (OR x y) 	= genError (evaluaCB  m x) (||) (evaluaCB m y)
evaluaCB m (NOT x)  	= notError (evaluaCB  m x)

--evaluaExpressionsNumeriques
--dona error si conte alguna variable sense assignar o si es produeix una divisio per zero.
evaluaNum :: (SymTable m, Num a, Ord a) => m a -> ExpNum a -> (Either a String)
evaluaNum m (Const x) 	= Left x
evaluaNum m (Var x) 	
	| exists m x 		= Left (value m x)
	| otherwise  		= Right ("Undefined variable: " ++ x)
evaluaNum m (Plus x y) 	= genError (evaluaNum m x) (+) (evaluaNum m y)
evaluaNum m (Minus x y) = genError (evaluaNum m x) (-) (evaluaNum m y)
evaluaNum m (Div x y)	= divError (evaluaNum m x) (evaluaNum m y)
evaluaNum m (Times x y) = genError (evaluaNum m x) (*) (evaluaNum m y)

genError (Left a) f (Left b)= Left (f a b)
genError (Right a) _  _ 	= Right a
genError _ _ (Right b) 		= Right b

notError :: (Either Bool String) -> (Either Bool String)
notError (Left a) 		= Left (not a)
notError (Right a) 		= Right a

divError :: (Num a, Ord a) => (Either a String) -> (Either a String) -> (Either a String)
divError (Right a) _ 	= Right a
divError _ (Right b) 	= Right b
divError (Left a) (Left b)
  | b == 0 				= Right ("division by zero")
  | otherwise 			= Left (mydiv a b)

--La divisio s’ha d’interpretar com el quocient (es a dir, la part entera)
mydiv :: (Num a, Ord a) => a -> a -> a
mydiv a b 
	| (a < 0 && b < 0)  = (mydiv2 (-a) (-b) 0)
	| (a < 0 && b > 0)	= (-1) * (mydiv2 (-a) b 0)
	| (a > 0 && b < 0)  = (-1) * (mydiv2 a (-b) 0)
	| otherwise 		= (mydiv2 a b 0)

mydiv2 :: (Num a, Ord a) => a -> a -> a -> a
mydiv2 a b 0 
	| a < b 			= 0
mydiv2 a b c
	| a < b 			= c
	| a >= b 			= mydiv2 (a-b) b (c+1)


interpretCommand :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommand m x c@(Assign a b)  		= interpretCommandAssign m x c 
interpretCommand m x c@(Print a)			= interpretCommandPrint  m x c 
interpretCommand m x (Seq a)  				= interpretCommandSeq m x (Seq a) []
interpretCommand m x c@(Input a) 			= interpretCommandInput  m x c 
interpretCommand m x d@(Cond a b c) 		= interpretCommandIf     m x d 
interpretCommand m x c@(Loop a b) 			= interpretCommandLoop  m x c

interpretCommandAssign :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommandAssign m x (Assign a b) 
    | (Right error)  <- aux = (Right error, m, x)
    | (Left valor) <- aux 	= (Left [], update m a valor, x)
    	where aux = evaluaNum m b

interpretCommandPrint :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommandPrint m x (Print a)
    | (Right error)  <- aux = (Right error, m, x)
    | (Left valor) <- aux 	= (Left [valor], m, x)
   		where aux = evaluaNum m a

interpretCommandInput :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommandInput m (x:xs) (Input a) 	= (Left [], update m a x, xs)

interpretCommandIf :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommandIf m x (Cond a b c)
    | (Right error) <- aux 	= (Right error, m, x)
    | (Left True) <- aux 	= interpretCommand m x b 
    | otherwise 			= interpretCommand m x c
   		where aux = evaluaCB m a

interpretCommandLoop :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommandLoop m x all@(Loop a b)
    | (Right error) <- aux 	=  (Right error, m, x)
    | (Left False) <- aux 	= (Left [], m, x)
    | otherwise 			= (asdf m x all [])
    	where aux = evaluaCB m a

asdf:: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> [a] -> ((Either [a] String),m a, [a])
asdf m x all@(Loop a z) b
  	| (Right s, m, x2) <- aux 		= (Right s, m, x2)
  	| (Left [], m, x2) <- aux 		= asdf m x2 (Seq all) b
  	| (Left [d], m, x2) <- aux 		= asdf m x2 (Seq $all) (b++[d])
  		where aux = interpretCommand m x a

interpretCommandSeq:: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> [a] -> ((Either [a] String),m a, [a])
interpretCommandSeq m x (Seq []) b 				= (Left b, m, x)
interpretCommandSeq m x (Seq(a:as)) b
  	| (Right s, m, x2) <- aux 		= (Right s, m, x2)
  	| (Left [], m, x2) <- aux 		= interpretCommandSeq m x2 (Seq as) b
  	| (Left [d], m, x2) <- aux 		= interpretCommandSeq m x2 (Seq as) (b++[d])
  		where aux = interpretCommand m x a


formatAux:: (SymTable m, Num a, Ord a) => ((Either [a] String),m a, [a]) -> (Either [a] String)
formatAux (Right x, m, a) 	= Right x
formatAux (Left[], m, a) 	= Left []
formatAux (Left [x], m, a) 	= Left [x]

interpretProgram:: (Num a,Ord a) => [a] -> Command a -> (Either [a] String)
interpretProgram x y 		= formatAux $ interpretCommand (start::Memoria1 y) x y

-------------------------------------------------------------
--4															|
-------------------------------------------------------------

--elimina les connectives AND i OR de les condicions dels ifthenelse introduint mes
--condicionals i mantenint el mateix comportament.

expand :: Command a -> Command a
expand (Seq []) 			= Seq []
expand (Seq (x:xs)) 		= Seq $ ([expand x] ++ [expand $ Seq xs])
expand (Loop a b)			= Loop a (expand b)
expand (Cond a b c) 		= descomprimeix a (expand b) (expand c)
expand x 					= x

descomprimeix :: ExpBool a -> Command a -> Command a -> Command a
descomprimeix (AND x y) a b = Cond x (descomprimeix y a b) b
descomprimeix (OR x y) a b 	= Cond x  a (descomprimeix y a b)
descomprimeix a b c			= Cond a b c

--que eliminat totes les instruccions sense bifurcacio i deixant un (Seq []) com a
--l'unic possible codi que no sigui un condicional o un loop.
simplify :: Command a -> Command a
simplify (Seq []) 			= Seq []
simplify (Seq (x:xs))		= Seq $ ([simplify x] ++ [simplify $ Seq xs])
simplify (Cond a b c)		= Cond a (simplify b) (simplify c)
simplify (Loop a b)			= Loop a (simplify b)
simplify _ 					= simplify $ Seq[]

-- Es isomorf si sense tenir en compte les condicions,
-- son iguals sota permutacio de la part del “then” i la part del “else” dels condicionals.
isomorf :: Command a -> Command a -> Bool 
isomorf (Seq[]) (Seq[]) 				= True
isomorf (Seq[]) _						= False
isomorf _ 	(Seq[])						= False	
isomorf (Cond a1 b1 c1) (Cond a2 b2 c2) = (isomorf b1 b2 && isomorf c1 c2) || (isomorf b1 c2 && isomorf c1 b2)
isomorf (Loop a1 b1) (Loop a2 b2)		= isomorf b1 b2
isomorf (Seq (x:xs)) (Seq (y:ys))		= (isomorf x y) && (isomorf (Seq xs) (Seq ys))
isomorf  x y 							= False				
