module Semantic where

type Identifier = String
data Expr = V Identifier | I Int | B Bool 
    | Add Expr Expr | Mul Expr Expr | Succ Expr
    | Pred Expr
    | Not Expr | And Expr Expr | Or Expr Expr
    | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
    | If Expr Expr Expr
    | Let Identifier Expr Expr 
    | Fn Identifier Expr
    | App Expr Expr deriving (Eq, Show)


--Borrar


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


--No borrar

eval1 :: Expr -> Expr
eval1 (V x) = (V x)
eval1 (I n) = (I n)
eval1 (B b) = (B b)
eval1 (Add (I n) (I m)) = (I (n+m))
eval1 (Add (I n) b) = (Add (I n) (eval1 b))
eval1 (Add a b) = (Add (eval1 a) b) 
eval1 (Mul (I n) (I m)) = (I (n*m))
eval1 (Mul (I n) b) = (Mul (I n) (eval1 b))
eval1 (Mul a b) = (Mul (eval1 a) b) 
eval1 (Succ (Pred (I n))) = (I (n))
eval1 (Succ (I n)) = (I (n+1))
eval1 (Succ a) = Succ (eval1 a)
eval1 (Pred (Succ (I n))) = (I (n))
eval1 (Pred (I 0)) = (I (0))
eval1 (Pred (I n)) = (I (n-1))
eval1 (Pred a) = (Pred (eval1 a))
eval1 (Not (B b)) = (B (not b))
eval1 (Not b) = (Not (eval1 b))
eval1 (And (B b) (B c)) = (B (b&&c))
eval1 (And (B b) c) = (And (B b) (eval1 c))
eval1 (And b c) = (And (eval1 b) c)
eval1 (Or (B b) (B c)) = (B (b||c))
eval1 (Or (B b) c) = (Or (B b) (eval1 c))
eval1 (Or b c) = (Or (eval1 b) c)
eval1 (Lt (I n) (I m)) = (B (n<m))
eval1 (Lt (I b) c) = (Lt (I b) (eval1 c))
eval1 (Lt b c) = (Lt (eval1 b) c)
eval1 (Gt (I n) (I m)) = (B (n>m))
eval1 (Gt (I b) c) = (Gt (I b) (eval1 c))
eval1 (Gt b c) = (Gt (eval1 b) c)
eval1 (Eq (I n) (I m)) = (B (n==m))
eval1 (Eq (I b) c) = (Eq (I b) (eval1 c))
eval1 (Eq b c) = (Eq (eval1 b) c)
eval1 (If (B b) x y)
    | b = x
    | otherwise = y
eval1 (If b x y) = (If (eval1 b) x y)
eval1 (Let x (I n) c) = (subst c (x,(I n)))
eval1 (Let x (B b) c) = (subst c (x,(B b)))
eval1 (Let x (Fn y e) c) = (subst c (x,(Fn y e)))
eval1 (Let x b c) = (Let x (eval1 b) c)
eval1 (App (Fn x e) e2) = subst e (x, e2) 
eval1 (App e1 e2) = App (eval1 e1) e2
eval1 (Fn x e) = Fn x (eval1 e)


evals :: Expr -> Expr
evals (V x) = V x
evals (I n) = I n
evals (B b) = B b
evals (Add (I n) (I m)) = (I (n+m))
evals (Add (I n) b) = evals (Add (I n) (evals b))
evals (Add a b) = eval1 (Add (evals a) b) 
evals (Mul (I n) (I m)) = (I (n*m))
evals (Mul (I n) b) = evals (Mul (I n) (evals b))
evals (Mul a b) = eval1 (Mul (evals a) b)
evals (Succ (Pred (I n))) = (I (n))
evals (Succ (I n)) = (I (n+1))
evals (Succ a) = eval1 (Succ (evals a))
evals (Pred (Succ (I n))) = (I (n))
evals (Pred (I 0)) = (I (0))
evals (Pred (I n)) = (I (n-1))
evals (Pred a) = eval1 (Pred (evals a))
evals (Not (B b)) = (B (not b))
evals (Not b) = eval1 (Not (evals b))
evals (And (B b) (B c)) = (B (b&&c))
evals (And (B b) c) = evals (And (B b) (evals c))
evals (And b c) = eval1 (And (evals b) c)
evals (Or (B b) (B c)) = (B (b||c))
evals (Or (B b) c) = evals (Or (B b) (evals c))
evals (Or b c) = eval1 (Or (evals b) c)
evals (Lt (I n) (I m)) = (B (n<m))
evals (Lt (I b) c) = evals (Lt (I b) (evals c))
evals (Lt b c) = eval1 (Lt (evals b) c)
evals (Gt (I n) (I m)) = (B (n>m))
evals (Gt (I b) c) = evals (Gt (I b) (evals c))
evals (Gt b c) = eval1 (Gt (evals b) c)
evals (Eq (I n) (I m)) = (B (n==m))
evals (Eq (I b) c) = evals (Eq (I b) (evals c))
evals (Eq b c) = eval1 (Eq (evals b) c)
evals (If (B b) x y)
    | b = evals x
    | otherwise = evals y
evals (If b x y) = evals (If (evals b) x y)
evals (Let x (I n) c) = evals (subst c (x,(I n)))
evals (Let x (B b) c) = evals (subst c (x,(B b)))
evals (Let x (Fn y e) c) =  evals (subst c (x,(Fn y e)))
evals (Let x b c) = evals (Let x (evals b) c)
evals (App (Fn x e) e2) = evals (subst e (x, e2)) 
evals (App e1 e2) = App (evals e1) e2
evals (Fn x e) = Fn x (evals e)


evale :: Expr -> Expr
evale (Add (B b) _) = error "[Add] Espera dos Numeros."
evale (Add _ (B b)) = error "[Add] Espera dos Numeros."
evale (Mul (B b) _) = error "[Mul] Espera dos Numeros."
evale (Mul _ (B b)) = error "[Mul] Espera dos Numeros."
evale (Succ (B b)) = error "[Succ] Espera un Numero."
evale (Pred (B b)) = error "[Succ] Espera un Numero."
evale (Not (I n)) = error "[Not] Espera un Booleano"
evale (And (I b) _) = error "[And] Espera dos Booleanos."
evale (And _ (I b)) = error "[And] Espera dos Booleanos."
evale (Or (I b) _) = error "[Or] Espera dos Booleanos."
evale (Or _ (I b)) = error "[Or] Espera dos Booleanos."
evale (Lt (B b) _) = error "[Lt] Espera dos Numero."
evale (Lt _ (B b)) = error "[Lt] Espera dos Numero."
evale (Gt (B b) _) = error "[Gt] Espera dos Numero."
evale (Gt _ (B b)) = error "[Gt] Espera dos Numero."
evale (Eq (B b) _) = error "[Eq] Espera dos Numero."
evale (Eq _ (B b)) = error "[Eq] Espera dos Numero."
evale (If (I b) _ _) = error "[If] Espera un Booleano."
evale (App (Fn x e) e2) = evale (subst e (x, e2))
evale (App e _) = error "[App] Espera una función como primer parámetro."
evale (Fn _ _) = error "[Fn] No tiene aplicación."
evale (V x) = V x
evale (I n) = I n
evale (B b) = B b
evale a = if valor (evals a) then (evals a) else evale (evals a)

valor :: Expr -> Bool
valor a = case a of 
    (I n) -> True
    (B n) -> True
    (V x) -> True
    otherwise -> False