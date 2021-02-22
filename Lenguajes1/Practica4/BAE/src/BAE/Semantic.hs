module BAE.Semantic where

-- Instruccion que importa modulos del modulo Sintax
import BAE.Sintax

-- Tipo de dato que define tipos para realizar su asignacion y su comparacion
data Type = Integer | Boolean deriving (Eq,Show)

-- Sinónimo de una tupla del Tipo Identifier y Type
type Decl = (Identifier,Type)

-- Sinónimo para la lista del tipo Decl
type TypCtxt = [Decl]

-- Funcion eval que realiza una evaliacion de la Expresion recibida
eval1 :: Expr -> Expr
eval1 (Add (I a) (I b)) = I (a+b) --esumaf
eval1 (Mul (I a) (I b)) = I(a*b) --emulf
eval1 (Add (I a) b) = Add (I a) (eval1 b) --esumad
eval1 (Mul (I a) b) = Mul (I a) (eval1 b) -- emuld
eval1 (Add a b) = Add (eval1 a) b --esumai
eval1 (Mul a b) = Mul (eval1 a) b --emuli
eval1 (Succ (I a)) = I (a + 1)  --esucn
eval1 (Pred (I a)) = I (a - 1) -- epreds
eval1 (Succ a) = Succ (eval1 a) -- esuc
eval1 (Pred a) = Pred (eval1 a) --epred
eval1 (Not (B a)) = B (not a) --enotf
eval1 (Not a) = Not (eval1 a) --enot
eval1 (And (B a) (B b)) = B (a&&b) --eandf
eval1 (Or (B a) (B b)) = B(a||b) --eorf
eval1 (And (B a) b) = And (B a) (eval1 b) --esandd
eval1 (Or (B a) b) = Or (B a) (eval1 b) -- eord
eval1 (And a b) = And (eval1 a) b --eandi
eval1 (Or a b) = Or (eval1 a) b --eori
eval1 (Lt (I a) (I b)) = B (a<b) --eltf
eval1 (Gt (I a) (I b)) = B (a>b) --egtf
eval1 (Eq (I a) (I b)) = B (a==b) --eeqf
eval1 (Lt (I a) b) = Lt (I a) (eval1 b) --eltd
eval1 (Gt (I a) b) = Gt (I a) (eval1 b) --egtd
eval1 (Eq (I a) b) = Eq (I a) (eval1 b) --eeqd
eval1 (Lt a b) = Lt (eval1 a) b --elti
eval1 (Gt a b) = Gt (eval1 a) b --egti 
eval1 (Eq a b) = Eq (eval1 a) b --eeqi
eval1 (If (B True) a b) =  a --eiftrue
eval1 (If (B False) a b) = b --eiffalse
eval1 (If a b c) = If (eval1 a) b c --eif
eval1 (Let x (Fix y e1) e2) = BAE.Sintax.subst e2 (x,(Fix y e1)) 
eval1 (Let x a b) = subst b (x,a) --elet
eval1 (Fn i e) = if(normal e)
                 then error "Bloqueada"
                 else (Fn i (eval1 e))
eval1 (Fix x e) = BAE.Sintax.subst e (x,(Fix x e))
eval1 (App (Fn i e1) x) = subst e1 (i,x)  
eval1 (App e1 e2)
  | (normal e1) = (App e1 (eval1 e2))
  | otherwise = (App (eval1 e1) e2)
eval1 e = error "estado bloqueado"

-- Funcion evals: realiza las evaluaciones varias evaluaciones hasta que sea una transicion bloqueada
evals :: Expr -> Expr
evals a
 | (esBloqueada a) = a
 | otherwise = evals (eval1 a)

-- Funcion evale: lanza los errores en los que al final de la evaluación si el final de la evaluación no es un valor
evale :: Expr -> Expr
evale a
 | esValor (evals a) = (evals a)
 | otherwise = error (errorEval (evals a))

-- Funcion vt: que realiza la verificacion de tipos de una expresión
vt :: TypCtxt -> Expr -> Type -> Bool
vt xs (V x) a = elem (x,a) xs --tvar
vt xs (I a) Integer = True --tnum
vt xs (B True) Boolean = True --ttrue
vt xs (B False) Boolean = True --tfalse
vt xs (Add t1 t2) Integer = (vt xs t1 Integer) && (vt xs t2 Integer) --tsum
vt xs (Mul t1 t2) Integer = (vt xs t1 Integer) && (vt xs t2 Integer) --tprod
vt xs (Succ t) Integer = (vt xs t Integer) --tsuc
vt xs (Pred t) Integer = (vt xs t Integer) --tpred
vt xs (Not t) Boolean = (vt xs t Boolean) -- tnot
vt xs (And t1 t2) Boolean = (vt xs t1 Boolean) && (vt xs t2 Boolean) --tand
vt xs (Or t1 t2) Boolean = (vt xs t1 Boolean) && (vt xs t2 Boolean) --tor
vt xs (Lt t1 t2) Boolean = (vt xs t1 Integer) && (vt xs t2 Integer) --tlt
vt xs (Gt t1 t2) Boolean = (vt xs t1 Integer) && (vt xs t2 Integer) --tgt
vt xs (Eq t1 t2) Boolean = (vt xs t1 Integer) && (vt xs t2 Integer) --teq
vt xs (If e t1 t2) Integer = (vt xs e Boolean) && (vt xs t1 Integer) && (vt xs t2 Integer) --tifnum
vt xs (If e t1 t2) Boolean = (vt xs e Boolean) && (vt xs t1 Boolean) && (vt xs t2 Boolean) --tifbool
vt xs (Let x t1 t2) a 
 | (vt xs t1 Boolean) && (vt ((x,Boolean):xs) t2 a) = True
 | (vt xs t1 Integer) && (vt ((x,Integer):xs) t2 a) = True
 | otherwise = False
vt _ _ _ = False

eval :: Expr -> Type -> Expr
eval e t
  | vt [] e t = evale e
  | otherwise = error "Exception: type error"

------------------------------- FUNCIONES AUXILIARES ------------------------------------

-- Funciona Auxiliar para la función evale que devuelve un mensaje de error especifico para todas las operaciones que tenemos definidas
errorEval:: Expr -> String
errorEval (Add a b) = "Error: Add expects two Integers"
errorEval (Mul a b) = "Error: Mul expects two Integers"
errorEval (Succ a) = "Error: Succ expects a Integers"
errorEval (Pred a) = "Error: Pred expects a Integers"
errorEval (Not a) = "Error: No expects a Boolean"
errorEval (And a b) = "Error: And expects two Booleans"
errorEval (Or a b) = "Error: Or expects two Booleans"
errorEval (Lt a b) = "Error: Lt expects two Integers" 
errorEval (Gt a b) = "Error: Gt expects two Integers"
errorEval (Eq a b) = "Error: Eq expects two Integers"
errorEval (If a b c) = "Error: If expects a Boolean"
errorEval (Let x b c) = "Error: Element of let is Block"
errorEval (App a b) = "Error: its not possible apply the function"
errorEval (Fn x a) = "Error: Missing argument"


-- Funcion Auxiliar para evals que devuelve un false si la expresión recibida cumple con las caracteristicas de ser bloqueada
esBloqueada :: Expr -> Bool
esBloqueada (Add (I a) (I b)) = False
esBloqueada (Mul (I a) (I b)) = False
esBloqueada (Add (I a) b) = (esBloqueada b)
esBloqueada (Mul (I a) b) = (esBloqueada b)
esBloqueada (Add a b) = (esBloqueada a)
esBloqueada (Mul a b) = (esBloqueada a)
esBloqueada (Succ (I a)) = False
esBloqueada (Pred (I a)) = False
esBloqueada (Succ a) = (esBloqueada a)
esBloqueada (Pred a) = (esBloqueada a)
esBloqueada (Not (B a)) = False
esBloqueada (Not a) = (esBloqueada a)
esBloqueada (And (B a) (B b)) = False
esBloqueada (Or (B a) (B b)) = False
esBloqueada (And (B a) b) = (esBloqueada b)
esBloqueada (Or (B a) b) = (esBloqueada b)
esBloqueada (And a b) = (esBloqueada a)
esBloqueada (Or a b) = (esBloqueada a)
esBloqueada (Lt (I a) (I b)) = False
esBloqueada (Gt (I a) (I b)) = False
esBloqueada (Eq (I a) (I b)) = False
esBloqueada (Lt (I a) b) = (esBloqueada b)
esBloqueada (Gt (I a) b) = (esBloqueada b)
esBloqueada (Eq (I a) b) = (esBloqueada b)
esBloqueada (Lt a b) = (esBloqueada a)
esBloqueada (Gt a b) = (esBloqueada a)
esBloqueada (Eq a b) = (esBloqueada a)
esBloqueada (If (B True) a b) =  False
esBloqueada (If (B False) a b) = False
esBloqueada (If a b c) = (esBloqueada a)
esBloqueada (Let x (Fix y e) _ ) = False
esBloqueada (Let x a b) = False
esBloqueada (Fn x e) = esBloqueada e
esBloqueada (Fix x e) =   True
esBloqueada (App (Fn x e) e1) = False
esBloqueada (App e1 e2) = (esBloqueada e1) && (esBloqueada e2)
esBloqueada a = True

-- Función auxiliar para la función evale que devuelve True si la expresion es un entero o un booleano, en cual quier otro caso
-- es decir si es una funcion la expresion recibida devuelve false
esValor :: Expr -> Bool
esValor (I a) = True
esValor (B a) = True
esValor (Fix x e) = True
esValor _ = False

normal :: Expr -> Bool
normal (V x) = True
normal (Fn x e) = normal e
normal (App e1 e2) = case e1 of
            (Fn x e) -> False 
            e -> (normal e1) && (normal e2)
normal (Fix x e) = False
normal _ = False

