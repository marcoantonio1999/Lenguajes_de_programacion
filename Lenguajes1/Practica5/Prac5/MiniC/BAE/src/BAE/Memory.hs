module BAE.Memory where
import BAE.Sintax

-- Alias for memory addresses.
type Address = Int
-- Alias for values.
type Value = Expr
type Cell = (Address,Value)
type Memory = [Cell]
--newAddress. Dada una memoria, genera una nueva dirección de memoria que no este contenida en esta.
newAddress :: Memory -> Expr
newAddress [] = L 0
newAddress l | norep (listaAdress l) == False = error "Memory Corrupted"
			 | otherwise  =  newAddress1 0 (listaAdress l) 

newAddress1 :: Int -> [Expr] -> Expr 
newAddress1  a  l |  elem  (L a) l == True = newAddress1 (a+1) l    
				  | otherwise = L a

--Lista de direcciones en la memoria

listaAdress :: Memory -> [Expr]
listaAdress [] = []
listaAdress  ((x,y):xs) = (L x):listaAdress xs

-- nos dice si hay números repetidos

norep :: Eq a => [a] -> Bool
norep [] = True
norep [x] = True
norep (x:xs) | (elem x xs == True) = False
	     | (elem x xs == False) = norep xs

-- Dada una dirección de memoria, devuelve el valor contenido en la celta con tal dirección, en caso de no encontrarla debe devolver Nothing.
access :: Address -> Memory -> Maybe Value
access a [] = Nothing
access a ((x,y):xs) | norep (listaAdress ((x,y):xs) ) == False = error "Memory Corrupted"
					| a == x = Just y
					| otherwise = access a xs 
--Dada una celda de memoria, actualiza el valor de esta misma en la memoria. En caso de no existir debe devolver Nothing.
update :: Cell -> Memory -> Maybe Memory
update a [] = Nothing
update (a, (Fn (e1)(e2) ) ) ((x,y):xs) =  Just (update1 (a, (Fn (e1)(e2) ) ) ((x,y):xs))
update (a, (Let (e1)(e2)(e3) ) ) ((x,y):xs) =  Just (update1 (a, (Let (e1)(e2)(e3) ) ) ((x,y):xs))

update (a,b) ((x,y):xs) | norep (listaAdress ((x,y):xs) ) == False = error "Memory Corrupted"
						| (frVars b /= [] ) = error "Memory can only store values"
						| otherwise = (update (a,b) ((x,y):xs) )
--funciń auxilar que se supone hace lo mimos que update, solo era para que saliera el just por el maybe
update1 :: Cell -> Memory ->  Memory

update1 (a, (Fn (e1)(e2) ) ) ((x,y):xs) | a == x = ((x, Fn (e1)(e2)):xs)     
								        | otherwise = ((x,y): update1 (a,Fn (e1)(e2)) xs)
update1 (a, (Let (e1)(e2)(e3) ) ) ((x,y):xs) | a == x = ((x, Let (e1)(e2)(e3)):xs)     
								        | otherwise = ((x,y): update1 (a,Fn (e1)(e2)) xs)
								       						
update1 (a,b) ((x,y):xs) | a == x =   ((x,b) : xs)  
					     | otherwise =  ((x,y): update1 (a,b) xs)




{-
update (a,Succ (I i)) ((x,y):xs) | a == x = (x,Succ (I i) )    
								 | otherwise update (x,y):(a,Succ (I i)) xs
update (a,Pred (I i)) ((x,y):xs) | a == x = (x,Pred (I i) )    
								 | otherwise update (x,y):(a,Pred (I i)) xs
update (a,I i) ((x,y):xs) 		 | a == x = (x, I i )    
								 | otherwise update (x,y):(a,I i) xs
update (a,B i) ((x,y):xs) 		 | a == x = (x, B i )    
								 | otherwise update (x,y):(a,B i) xs
update (a,Not (B i)) ((x,y):xs)  | a == x = (x, Not (B i) )    
								 | otherwise update (x,y):(a,Not (B i)) xs

update (a, Add (I b)(I c)) ((x,y):xs) | a == x = (x,Add (I b)(I c) )    
								      | otherwise update (x,y):(a,Add (I b)(I c)) xs
update (a, Mul (I b)(I c)) ((x,y):xs) | a == x = (x,Mul (I b)(I c) )    
								      | otherwise update (x,y):(a,Mul (I b)(I c)) xs
update (a, And (B b)(B c)) ((x,y):xs) | a == x = (x,And (I b)(I c) )    
								      | otherwise update (x,y):(a,And (I b)(I c)) xs
update (a, Or (B b)(B c)) ((x,y):xs)  | a == x = (x,Or (I b)(I c) )    
								      | otherwise update (x,y):(a,Or (I b)(I c)) xs
update (a, Lt (I b)(I c)) ((x,y):xs)  | a == x = (x,Lt (I b)(I c) )    
								      | otherwise update (x,y):(a,Lt (I b)(I c)) xs
update (a, Gt (I b)(I c)) ((x,y):xs)  | a == x = (x,Gt (I b)(I c) )    
								      | otherwise update (x,y):(a,Gt (I b)(I c)) xs
update (a, Eq (I b)(I c)) ((x,y):xs)  | a == x = (x,Eq (I b)(I c) )    
								      | otherwise update (x,y):(a,Eq (I b)(I c)) xs

update (a, If (B b)(B c)(e)) ((x,y):xs)   | a == x = (x, If (B b)(B c)(B d) )    
								          | otherwise update (x,y):(a,If (B b)(B c)(B d)) xs

update (a, If (B b)(B c)(e)) ((x,y):xs)   | a == x = (x, If (B b)(B c)(B d) )    
								          | otherwise update (x,y):(a,If (B b)(B c)(e)) xs

update (a,Let (a)(e)(e1) )  ((x,y):xs)    | a == x = (x, Let (a)(e)(e2))    
								          | otherwise update (x,y):(a,Let (a)(e)(e2)) xs
update (a,Fn (a)(e))  ((x,y):xs)          | a == x = (x, Fn (a)(e))    
								          | otherwise update (x,y):(a,Fn (a)(e)) xs
update (a, App (e)(e1)) ((x,y):xs)        | a == x = (x,App (e)(e1) )    
								          | otherwise update (x,y):(a,App (e)(e1)) xs
update (a, e) ((x,y):xs)                  | frVars e = [] == False =     
								          | otherwise update (x,y):(a,App (e)(e1)) xs
-}


