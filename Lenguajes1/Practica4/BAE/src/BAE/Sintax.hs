module BAE.Sintax where

import Data.Char
import Data.List

-- Sinonimo para el identificador de una variable
type Identifier =  String

-- Tipo de dato que define las expresiones Aritmeticas y Booleanas (EAB)
data Expr = V Identifier | I Int | B Bool
           | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
           | Not Expr | And Expr Expr | Or Expr Expr
           | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
           | If Expr Expr Expr
           | Let Identifier Expr Expr
           | Fn Identifier Expr
           | Fix Identifier Expr
           | App Expr Expr deriving (Eq)

{-- 
Instancia Show de Expr para represntar la sintaxis de orden superior de
las expresiones Aritmeticas y Booleanas (EAB)
--}
instance Show Expr where
    show e = case e of
        (V x) -> "V["++ x ++"]"
        (I n) -> "N["++ (show n) ++ "]"
        (B b) -> "B["++ (show b) ++ "]"
        (Add a b) -> "suma("++(show a)++","++(show b)++")"
        (Mul a b) -> "prod("++(show a)++","++(show b)++")"
        (Succ a) -> "succ("++(show a)++")"
        (Pred a) -> "pred("++(show a)++")"
        (Not a) -> "not("++(show a)++")"
        (And a b) -> "and("++(show a)++","++(show b)++")"
        (Or a b) -> "or("++(show a)++","++(show b)++")"
        (Lt a b) -> "lt("++(show a)++","++(show b)++")"
        (Gt a b) -> "gt("++(show a)++","++(show b)++")"
        (Eq a b) -> "eq("++(show a)++","++(show b)++")"
        (If a b c) -> "if("++(show a)++","++(show b)++","++(show c)++")"
        (Let i a b) -> "let("++(show a)++","++(show i)++"."++(show b)++")"
        (Fn x e) -> "fn(" ++ x ++ "." ++ (show e)++ ")"
        (App a b) -> "app("++ (show a) ++ ", "++(show b)++")"
        (Fix x e) -> "fix(" ++ x ++ "." ++ (show e)++ ")"


-- Sinonimo para representar una substitución   
type Substitution = (Identifier,Expr)

{--
Función frVars, Se encarga de hacer una lista de todas las variables libres de una expresión
eliminando elementos repetidos.
--}
frVars :: Expr -> [Identifier]
frVars (V x) = [x]
frVars (Add a b) = delRep (frVars a ++ frVars b)
frVars (Mul a b) = delRep (frVars a ++ frVars b)
frVars (And a b) = delRep (frVars a ++ frVars b)
frVars (Or a b) = delRep (frVars a ++ frVars b)
frVars (Lt a b) = delRep (frVars a ++ frVars b)
frVars (Gt a b) = delRep (frVars a ++ frVars b)
frVars (Eq a b) = delRep (frVars a ++ frVars b)
frVars (Succ a) = delRep (frVars a)
frVars (Pred a) = delRep (frVars a)
frVars (Not a) = delRep (frVars a)
frVars (If a b c) = delRep (frVars a ++ frVars b ++ frVars c)
frVars (Let i a b) = delRep (frVars a ++ (Prelude.filter (/= i) (frVars b)))
frVars (Fn x e) = (Prelude.filter (/= x) (frVars e))
frVars (App a b) = delRep (frVars a ++ frVars b)
frVars (Fix x e) = (Prelude.filter (/= x) (frVars e))
frVars _ = []

{--
Función subst, recibe una expresión y una substitución y devuelve la instrucción
resultante de aplicar la substitución. Para el caso de let no se hace uso de las alpha
equivalencias  para resolver el problema de la parcialidad de la función. 
--}
subst:: Expr -> Substitution -> Expr
subst (V x1) (x2,r) = if (x1 == x2)
                      then r
                      else (V x1)
subst (Add a b) s = Add (subst a s) (subst b s)
subst (Mul a b) s = Mul (subst a s) (subst b s)
subst (And a b) s = And (subst a s) (subst b s)
subst (Or a b) s = Or (subst a s) (subst b s)
subst (Lt a b) s = Lt (subst a s) (subst b s)
subst (Gt a b) s = Gt (subst a s) (subst b s)
subst (Eq a b) s = Eq (subst a s) (subst b s)
subst (Succ a) s = Succ (subst a s)
subst (Pred a) s = Pred (subst a s)
subst (Not a) s = Not (subst a s)
subst (If a b c) s = If (subst a s) (subst b s) (subst c s)
subst (Let z a e) (x,r)  
 | z == x  = Let z a e
 | elem z (frVars r) = subst (alphaExpr (Let z a e)) (x,r) 
 | otherwise = Let z (subst a (x,r)) (subst e (x,r))
subst (App e1 e2) (x,a) = App (subst e1 (x,a)) (subst e2 (x,a)) 
subst (Fn i e) (x,a)
   | x == i = Fn i e
   | elem i (frVars a) = subst (alphaExpr (Fn i e)) (x,a)
   | otherwise = Fn i (subst e (x,a))
subst (Fix i e) (x,a)
   | x == i = Fix i e
   | elem i (frVars a) = subst (alphaExpr (Fix i e)) (x,a)
   | otherwise = Fix i (subst e (x,a))
subst e s = e

{--
Funcion alphaEq, recibe dos expresiones y nos dice si son alpha equivalencias.
return true sin son alpha equivalencias y false si no lo son.
--}
alphaEq:: Expr -> Expr -> Bool
alphaEq (V x1) (V x2)= x1==x2
alphaEq (I a) (I b) = a==b
alphaEq (B a)(B b) = a == b
alphaEq (Add a b)(Add c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Mul a b)(Mul c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (And a b)(And c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Or a b)(Or c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Lt a b)(Lt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Gt a b)(Gt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Eq a b)(Eq c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Succ a)(Succ b) = alphaEq a b
alphaEq (Pred a)(Pred b) = alphaEq a b
alphaEq (Not a)(Not b) = alphaEq a b
alphaEq (If a b c)(If d e f) = (alphaEq a d) && (alphaEq b e) && (alphaEq c f)
alphaEq (Let i1 a1 b1)(Let i2 a2 b2) = (alphaEq a1 a2) && (alphaEq b2 (subst b1 (i1,V i2)))
alphaEq (Fn x1 e1) (Fn x2 e2) = alphaEq e1 (subst e2 (x2, V x1))
alphaEq (Fix x1 e1) (Fix x2 e2) = alphaEq e1 (subst e2 (x2, V x1))
alphaEq _ _ = False

------------------------------- FUNCIONES AUXILIARES ------------------------------------

{--
Funcion delRep, eliminas los elementos repetidos de una lista
--}
delRep:: (Eq a) => [a] -> [a]
delRep [] = []
delRep (x:xs) = x: delRep ( Prelude.filter (/= x) xs)

{-
Funcion increVar que incrementa el numero de la variable recibida
-}
incrVar :: Identifier -> Identifier
incrVar s 
 | (Data.List.last s) == '9' = (incrVar (Data.List.take ((Data.List.length s) -1)  s)) ++ "0"
 | Data.Char.isDigit (Data.List.last s) =  (Data.List.take ((Data.List.length s)-1) s) ++ Data.Char.intToDigit(Data.Char.digitToInt(Data.List.last s)+1):[]
 | otherwise = s ++ "1"


findVar:: Identifier -> Expr ->Identifier
findVar x e = let xn = (incrVar x) in
              if elem xn (frVars e)
              then findVar xn e 
              else xn

alphaExpr:: Expr -> Expr
alphaExpr (Let x e1 e2) = let t = (findVar x e2) in Let t e1 (subst e2 (x,(V t)))
alphaExpr (Fix x e) = let t = (findVar x e) in Fix t (subst e (x,(V t)))
alphaExpr (Fn x e) = let t = (findVar x e) in Fn t (subst e (x,(V t)))
alphaExpr a =  a