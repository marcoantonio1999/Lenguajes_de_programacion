module Sintax where

type Identifier = String
data Expr = V Identifier | I Int | B Bool 
    | Add Expr Expr | Mul Expr Expr | Succ Expr
    | Pred Expr
    | Not Expr | And Expr Expr | Or Expr Expr
    | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
    | If Expr Expr Expr
    | Let Identifier Expr Expr 
    | Fn Identifier Expr
    | App Expr Expr deriving (Eq)

instance Show Expr where
    show a = showA a

showA :: Expr -> String
showA a = case a of 
    (V x ) -> "V[" ++ show x ++ "]"
    (I n ) -> "I[" ++ show n ++ "]"
    (B b ) -> "B[" ++ show b ++ "]"
    (Add a b ) -> "Add (" ++ (showA a) ++ ", " ++ (showA b) ++ ")"
    (Mul a b ) -> "Mul (" ++ (showA a) ++ ", " ++ (showA b) ++ ")"
    (Succ b ) -> " s( " ++ (showA b) ++ " ) "
    (Pred p ) -> "Pred (" ++ showA p ++ ")"
    (Not p ) -> "Not (" ++ showA p ++ ")"
    (And p q ) -> "And (" ++(showA p) ++ ", " ++ (showA q) ++ ")"
    (Or p q ) -> "Or (" ++(showA p) ++ ", " ++ (showA q) ++ ")"
    (Lt p q ) -> "Lt (" ++(showA p) ++ ", " ++ (showA q) ++ ")"
    (Gt p q) -> "Gt (" ++(showA p) ++ ", " ++ (showA q) ++ ")"
    (Eq p q) -> "Eq (" ++(showA p) ++ ", " ++ (showA q) ++ ")"
    (If a b c) -> "if (" ++ (showA a) ++ ", " ++ (showA b) ++ ", " ++ (showA c) 
    (Let a b c) -> "Let (" ++ (showA b) ++ ", " ++ a ++"." ++ (showA c) ++ ")"
    (Fn x e) -> "fn(" ++ x ++ "." ++ (showA e) ++ ")"
    (App e1 e2) -> "app(" ++ (showA e1) ++ ", " ++ (showA e2) ++ ")"

type Substitution = ( Identifier , Expr )

rm :: Identifier -> [Identifier] -> [Identifier]
rm _ [] = []
rm a (x:xs) 
    | a == x = xs
    | otherwise = x:(rm a xs) 

frVars :: Expr -> [Identifier]
frVars a = case a of
    (V x) -> [x]
    (I n) -> []
    (B b) -> []
    (Add a b ) -> (frVars a) ++ (frVars b)
    (Mul a b ) -> (frVars a) ++ (frVars b)
    (Succ b ) -> (frVars b) 
    (Pred p ) -> (frVars p)
    (Not p ) -> (frVars p)
    (And p q ) -> (frVars p) ++ (frVars q)
    (Or p q ) -> (frVars p) ++ (frVars q)
    (Lt p q ) -> (frVars p) ++ (frVars q)
    (Gt p q) -> (frVars p) ++ (frVars q)
    (Eq p q) -> (frVars p) ++ (frVars q)
    (If a b c) -> (frVars a) ++ (frVars b) ++ (frVars c)
    (Let a b c) -> (frVars b) ++ (rm a (frVars c))
    (Fn x e) -> (rm x (frVars e))
    (App e1 e2) -> (frVars e1) ++ (frVars e2)

--Obtiene la parte numérica de una cadena
getNum :: String -> String
getNum [] = ""
getNum s = reverse (getNumAux (reverse s))

--Toma los ultimos elementos de la cadena si son numeros
getNumAux :: String -> String
getNumAux [] = ""
getNumAux (x:xs)
    | elem x "0123456789" = x:(getNumAux xs)
    | otherwise = ""

--Obtiene la parte alfabetica de una cadena
getString :: String -> String
getString x = (takeWhile (`notElem` (getNum x)) x)

--Toma una expresi ́on que involucre el ligado de una
--variable y devuelve una α-equivalente utilizando la funci ́on incrVar hasta
--encontrar un nombre que no aparezca en el cuerpo.
alphaExpr :: Expr -> Expr
alphaExpr a =  case a of
    (V x) -> V x
    (I n) -> (I n)
    (B b) -> (B b)
    (Add a b ) -> Add (alphaExpr a) (alphaExpr b)
    (Mul a b ) -> Mul (alphaExpr a) (alphaExpr b)
    (Succ b ) -> (Succ (alphaExpr b)) 
    (Pred p ) -> (Pred (alphaExpr p))
    (Not p ) -> (Not (alphaExpr p))
    (And p q ) -> And (alphaExpr p) (alphaExpr q)
    (Or p q ) -> Or (alphaExpr p) (alphaExpr q)
    (Lt p q ) -> Lt (alphaExpr p) (alphaExpr q)
    (Gt p q) -> Gt (alphaExpr p) (alphaExpr q)
    (Eq p q) -> Eq (alphaExpr p) (alphaExpr q)
    (Let x b c) -> auxAlpha (Let x (alphaExpr b) (alphaExpr c)) (x,V (incrVar x))
    (App e1 e2) -> App (alphaExpr e1) (alphaExpr e2)
    (Fn x e) -> auxAlpha (Fn x (alphaExpr e)) (x,V (incrVar x))

--Sustituye incluyendo Lambdas
auxAlpha :: Expr -> Substitution -> Expr
auxAlpha (V x) (y,(V z))
    | x==y = (V z)
    | otherwise = (V x)
auxAlpha (App e1 e2) s = App (auxAlpha e1 s) (auxAlpha e2 s)
auxAlpha (Fn x e) (y, (V z))
    | x==y = Fn z (auxAlpha e (y, (V z)))
    | otherwise = Fn x (auxAlpha e (y, (V z)))

--Dado un identificador, si este no termina en número
--le agrega el sufijo 1, en caso contrario toma el valor del n ́umero y lo
--incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar x
    | getNum x == "" = x++"1"
    | otherwise = (getString x) ++ show ((read (getNum x) :: Int )+1)

subst :: Expr -> Substitution -> Expr
subst a (y,z) = case a of 
    (V x) -> if x == y then z else (V x)
    (I n) -> a
    (B b) -> a
    (Add a b ) -> Add (subst a (y,z)) (subst b (y,z))
    (Mul a b ) -> Mul (subst a (y,z)) (subst b (y,z))
    (Succ b ) -> Succ (subst b (y,z)) 
    (Pred p ) -> Pred (subst p (y,z)) 
    (Not p ) -> Not (subst a (y,z)) 
    (And p q ) -> And (subst p (y,z)) (subst q (y,z))
    (Or p q ) -> Or (subst p (y,z)) (subst q (y,z))
    (Lt p q ) -> Lt (subst p (y,z)) (subst q (y,z))
    (Gt p q) -> Gt (subst p (y,z)) (subst q (y,z))
    (Eq p q) -> Eq (subst p (y,z)) (subst q (y,z))
    (If a b c) -> If (subst a (y,z)) (subst b (y,z)) (subst c (y,z)) 
    (Let a b c) -> if a == y ||  (elem a (frVars c))
        then subst (alphaExpr (Let a b c)) (incrVar y, z)
        else (if (elem a (frVars z)) then subst (alphaExpr (Let a b c)) (incrVar y, z) else Let a (subst b (y,z)) (subst c (y,z)))
    (App e1 e2) -> App (subst e1 (y,z)) (subst e2 (y,z))
    (Fn x e) -> if x == y ||  (elem x (frVars z))
        then subst (alphaExpr (Fn x e)) (incrVar y, z)
        else (if (elem x (frVars z)) then subst (alphaExpr (Fn x e)) (incrVar y, z) else Fn x (subst e (y,z)))


alphaEq :: Expr -> Expr -> Bool
alphaEq (V x) (V y) = True
alphaEq (I x) (I y) = if x == y then True else False
alphaEq (B x) (B y) = if x == y then True else False
alphaEq (Succ b ) (Succ c ) = (alphaEq b c) 
alphaEq (Add a b ) (Add c d ) = (alphaEq a c) && (alphaEq b d)
alphaEq (Mul a b ) (Mul c d ) = (alphaEq a c) && (alphaEq b d)
alphaEq (Pred p ) (Pred q ) = alphaEq p q
alphaEq (Not p ) (Not q ) = alphaEq p q
alphaEq (And p q ) (And r s ) = alphaEq p r && alphaEq q s 
alphaEq (Or p q ) (Or r s ) = alphaEq p r && alphaEq q s 
alphaEq (Lt p q ) (Lt r s ) = alphaEq p r && alphaEq q s 
alphaEq (Gt p q) (Gt r s ) = alphaEq p r && alphaEq q s 
alphaEq (Eq p q) (Eq r s ) = alphaEq p r && alphaEq q s 
alphaEq (Let a b c) (Let x y z) = (frVars (Let a b c)) == (frVars (Let x y z)) && (alphaEq c z)
