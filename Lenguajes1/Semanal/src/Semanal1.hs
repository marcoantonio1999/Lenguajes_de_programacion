module Semanal1 where
	
type Identifier = String
data Expr = V Identifier | I Int | B Bool
			| Add Expr Expr | Mul Expr Expr | Succ Expr
			| Pred Expr
			| Not Expr | And Expr Expr | Or Expr Expr
			| Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
			| If Expr Expr Expr
			| Let Identifier Expr Expr 
			deriving (Eq)

instance Show Expr where 
	show	(V x )      = " V    [" ++    x ++             "] "
	show	(I n )      = " N    [" ++ (show n) ++         "] "
	show	(B b )      = " B    [" ++ (show b) ++         "] "
	show	(Add a b)   = "ADD   ["++ (show a) ++","++ (show b) ++"]"
	show	(Mul a b)   = "MUL   ["++ (show a) ++","++ (show b) ++"]"
	show	(Eq a b)    = "Eq    ["++ (show a) ++","++ (show b) ++"]"
	show	(And a b)   = "And   ["++ (show a) ++","++ (show b) ++"]"
	show	(Or  a b)   = "Or    ["++ (show a) ++","++ (show b) ++"]"
	show	(Gt a b)    = "Gt    ["++ (show a) ++","++ (show b) ++"]"
	show	(Lt a b)    = "MUL   ["++ (show a) ++","++ (show b) ++"]"
	show	(Not a)	    = "Not   ["++ (show a) ++          "]" 
	show	(If a b c)  = "If    ["++ (show a) ++","++ (show b) ++","++ (show c) ++ "]"
	show	(Let x a b) = "Let   ["++     x    ++","++ (show a) ++","++ (show b) ++ "]"
	show	(Succ a)    = "Succ  ["++ (show a) ++  "]"
	show	(Pred a)    = "Pred  ["++ (show a)  ++ "]"
			



type Substitution = ( Identifier , Expr )	

frVars :: Expr -> [Identifier]
frVars (I i) = []
frVars (B b) = []
frVars (V r) = [r]
frVars (Add a b)  = (frVars a) ++ (frVars b) 
frVars (Mul a b)  = (frVars a) ++ (frVars b)
frVars (Succ a)   = (frVars a) 
frVars (Pred a)   = (frVars a)
frVars (Not  a)   = (frVars a)
frVars (And a b)  = (frVars a) ++ (frVars b)
frVars (Or a b)   = (frVars a) ++ (frVars b)
frVars (Lt a b)   = (frVars a) ++ (frVars b)
frVars (Gt a b)   = (frVars a) ++ (frVars b)
frVars (Eq a b)   = (frVars a) ++ (frVars b)
frVars (If a b c) = (frVars a) ++ (frVars b) ++ (frVars c) 
frVars (Let a b c) =(if ((frVars b) == (frVars c)) then (frVars b) else (frVars b ++ frVars c))


subst :: Expr -> Substitution -> Expr
subst (B b)  _ = (B b)
subst (I i)  _ = (I i)
subst (V s) (a, e) = if s == a then e else error "No se puede sustituir"
subst (Add e e1) (a, e2) = (Add(subst e (a ,e2)) (subst e1 (a ,e2)))
subst (Mul e e1) (a ,e2) = (Mul(subst e (a ,e2)) (subst e1 (a ,e2)))
subst (And e e1) (a ,e2) = (And(subst e (a ,e1)) (subst e1 (a ,e2)))
subst (Lt  e e1) (a ,e2) = (Lt (subst e (a ,e1)) (subst e1 (a ,e2)))
subst (Gt  e e1) (a ,e2) = (Gt (subst e (a ,e1)) (subst e1 (a ,e2)))
subst (Eq  e e1) (a ,e2) = (Eq (subst e (a ,e1)) (subst e1 (a ,e2)))
subst (Not e)    (a ,e2) = (Not(subst e (a ,e2)))
subst (Succ e)   (a ,e2) = (Succ(subst e (a ,e2)))
subst (If j e e1) (a ,e2)  = (If (subst j (a ,e2)) (subst e1 (a ,e2)) (subst e2 (a ,e2)))
--subst (Let a b c )(j, e2)
	

alphaEq :: Expr -> Expr -> Bool
alphaEq (V a) (V b)         = a==b
alphaEq (B a) (B b)         = a==b
alphaEq (I i) (I k)         = i==k  
alphaEq (Add a b) (Add c d) = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Mul a b) (Mul c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (And a b) (And c d) = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Or  a b) (Or c d)  = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Lt  a b) (Lt c d)  = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Gt  a b) (Gt c d)  = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Eq  a b) (Eq c d)  = (alphaEq a c) && (alphaEq b d)   	
alphaEq (Not a)   (Not c ) =  (alphaEq a c)
alphaEq (Succ a)  (Succ c) =  (alphaEq a c)
alphaEq (If a b c)(If d e f) = (alphaEq a d) && (alphaEq b e) && (alphaEq c f)
alphaEq (Let i a b) (Let j c d) = (not (elem i (frVars d))) && (not (elem j (frVars b))) && (alphaEq a c) && (alphaEq b d)




al :: [Int] -> (Int, Int)
al [] = error"no hay elementos"
al (x:[]) = error "no hay elementos"
al (x,y) = [x]++[y]
al (x:y:(z:zs)) 
	 | z>y = al (y:(z:zs))
	 | ((z<y) && (z>x)) = al (z:y:zs)
	 |otherwise = al (x:y:zs)

al2 :: [Int]-> [Int]
al2 x = if longitud x == 2 then x else al x


longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs