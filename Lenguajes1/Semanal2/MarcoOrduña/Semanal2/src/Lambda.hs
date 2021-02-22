module Lambda where
import GHC.Base
import GHC.Char
import GHC.Real (fromIntegral)
import GHC.Show
import GHC.Read (readLitChar, lexLitChar)
import GHC.Unicode
import GHC.Num
--Todas las bibliotecas que se importaron solo sirven para poder cambiar de String a Int y al revez 

type Identifier = String
data Expr = V Identifier
			| L Identifier Expr
			| App Expr Expr
		    deriving (Eq)
-- instancia de la Clase Show para poder mostrar en la teriminal  de la forma de haskell
instance Show Expr where
	show (V x)     = x
	show (App x y) = "("++ (show x) ++" "++(show y)++")"
	show (L v e)   = "/"++v++ "->"++ (show e) -- no utilice la otra diagonal invretida ya que me mandaba un error lexico

type Substitution = (Identifier,Expr)

{-Obtiene el conjunto de variables libres de una expresión.-}
frVars :: Expr -> [Identifier]
frVars    (V x)       = [x]
frVars    (App x y)   = (frVars x) ++ (frVars y)
frVars    (L v (V x)) = if ((v == x) == True) then [] else [x]
frVars    (L v (App (V a)(V b))) | ((a == v) && (v == b)) = []
								 | (v == a) = [b]
								 | (v == b) = [a]
								 | otherwise = [a,b] 
frVars    (L v e)     =  (quit v (frVars e))   
{-Quita todas las apariciones del identificador que le pasemos de una lista de identificadores-}
quit :: Identifier -> [Identifier] -> [Identifier]
quit  a [] = []
quit a (x:xs) | a==x = quit a xs
			  |otherwise = [x]++quit a xs
{-saca todas los identificadores de una abstracción lambda-}
listV :: Expr -> [Identifier]
listV (V x)     = []
listV (App x y) = listV x ++ listV y
listV (L x e)   = [x] ++ listV e
{-saca todas las variables de una expresión sin las que estan ligadas por la abtracción lambda-}
listV1 :: Expr -> [Identifier]
listV1 (V x)     = [x]
listV1 (App x y) = listV1 x ++ listV1 y
listV1 (L x y)   = listV1 y  
{-incrementa el valor si el identificador termina en un número  si no le agrega un uno-}
incrVar :: Identifier -> Identifier
incrVar a | (terminaNum (reversa a)) == True = obtnerChars a ++  show((cadenaAentero(reversa(reversa(obtnerNum(reversa a)))))+1)
		  | otherwise = a ++ "1" 
{-nos dice si el identificador termina en numero-}
terminaNum :: Identifier -> Bool
terminaNum [] = False
terminaNum (x:xs) | (digitToInt x) == 17 = False 
				  | otherwise =  True  
{-Obtiene los primeros caracteres antes de que aparescan los números-}
obtnerChars :: Identifier -> String
obtnerChars  [] = error "errorWithoutStackTrace"
obtnerChars  (x:xs) | elem (digitToInt x) [10..17] == True = [x]++ obtnerChars xs -- si el digito esta entre [10-17] quiere decir que es una letra en hexadecimal o es una letra si es 17  
					| otherwise = []

{-Cambia un char en un entero, si el char es una letra y no un numero regresa 17 ademas de que los digitos estan en hexadecimal-}
digitToInt :: Char -> Int
digitToInt c
  | (fromIntegral dec::Word) <= 9 = dec
  | (fromIntegral hexl::Word) <= 5 = hexl + 10
  | (fromIntegral hexu::Word) <= 5 = hexu + 10
  | otherwise = 17
  where
    dec = ord c - ord '0'
    hexl = ord c - ord 'a'
    hexu = ord c - ord 'A'
{-voltea un identificador al reves todos los caracteres-}
reversa :: Identifier -> Identifier
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]
{-obtiene los numeros en los que termina el identificador-}
obtnerNum :: Identifier -> String
obtnerNum []     = error "errorWithoutStackTrace"
obtnerNum (x:xs) | (elem (digitToInt x) [0..9] == True) = [x] ++ obtnerNum xs 
				 | otherwise = []  
{-cambia un String a un entero-}
cadenaAentero :: [Char] -> Int
cadenaAentero []     = 0
cadenaAentero (x:xs) = (cadenaAentero xs)*10 + (digitToInt x)  

{-recibe un identificador y una expresión y lo que hace es incrementar todas las apraciones segun el identificador que se le paso-}

incEx :: Identifier -> Expr -> Expr
incEx x (V a) = if (a==x)==True then V (incrVar a) else (V a)
incEx x (App a b) = App (incEx x a) (incEx x b )
incEx x (L i v)   = if ((x == i) == True) then (L (incrVar i) (incEx x v)) else (L i (incEx x v) )
{-lo mismo que la función pasada solo que recibe dos identificadores ya que  pueden haber dos abtraciones lambda ligando a una expresión-}
incEx1 :: Identifier -> Identifier-> Expr-> Expr
incEx1 x y (V a) | x == a = V (incrVar a)
				 | y == a = V (incrVar a)
				 | otherwise = (V a)
incEx1 x y (App a b) = App (incEx1 x y a) (incEx1 x y b)
incEx1 x y (L i e)   | x == i = L (incrVar i) (incEx1 x y e)
					 | y == i = L (incrVar i) (incEx1 x y e)
					 | otherwise = incEx1 x y e


{-Toma una expresión lambda y devuelve una α-
equivalente utilizando la función incrVar hasta encontrar un nombre que

no aparezca en el cuerpo.-}
alphaExpr :: Expr -> Expr
alphaExpr  (V x)       = (V x)
alphaExpr  (App x y)   = App (alphaExpr x) (alphaExpr y)
alphaExpr  (L i (App (V x)(V  y))) =  (L i (App (V x)(V  y)))
alphaExpr  (L i (V x)) = if i == x then L (incrVar i) (V (incrVar x) ) else L (incrVar i) (V (incrVar x) )  
alphaExpr  (L i (App (V x)(a))) =if i == x then L (incrVar i) (App (V (incrVar x) )(alphaExpr a)) else (L i (App (V x)(alphaExpr a)))
alphaExpr  (L i (App (a)(V y))) =if i == y then L (incrVar i) (App (alphaExpr a)(V (incrVar y) )) else (L i (App (alphaExpr a)(V y)))
alphaExpr  (L i (L i2 e )) = if (incrVar i==i2) == True && ((elem i (frVars e)) == True) then L (incrVar i) (L (incrVar i2) (incEx1 i i2 e)) else (L i (L i2 e ))
alphaExpr  (L i e)  = if ((elem i (frVars e)) == True) then L (incrVar i) (incEx i e) else (L i e)
   
{-Aplica la sustitución a la expresión dada.-}
subst :: Expr -> Substitution -> Expr
subst  (V x)     (i, (V v)) = if x == i then (V v) else (V x)
subst  (V x)     (i, ex)    = if x == i then ex else (V x)
subst  (App x y) (i, e)     = App (subst x (i,e)) (subst y (i,e))
subst  (L id e ) (i,(V x))  = (if x == id then (L ( incrVar id ) (subst e (i,(V x)))) else L id (subst e (i,(V x))))
subst  (L id e ) (i,ex)      = if ((elem id (listV ex ++ listV1 ex)) == True )then (L  id  (subst e (i,ex))) else (L id (subst e (i,ex)))
{-Implementa la β-reducción.-}
beta :: Expr -> Expr
beta (V x) = (V x)
beta (App (L a n)(e2)) = subst (beta n) (a,e2) 
beta (L x e) = L x  (beta e)
beta (App e1 e2) = App  (beta e1) (beta e2)
{-Determina si una expresión est ́a en forma normal, es
decir, no se pueden hacer más beta reducciones.-}
normal :: Expr -> Bool
normal e = 	e == beta(e)
{-Evalúa una expresión lambda implementando las reglas
definidas-}

eval :: Expr -> Expr
eval e  | (normal e == False) = eval(beta e)
		| otherwise = e  	