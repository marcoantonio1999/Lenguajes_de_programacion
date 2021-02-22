module BAE.Semantic where

import BAE.Sintax
data Type = Integer | Boolean deriving (Eq,Show)
type Decl = (Identifier,Type)
type TypCtxt = [Decl]

--No borrar

eval1 :: (Memory, Expr) -> (Memory, Expr)
eval1 (m, (V x)) = (m, (V x))
eval1 (m, (I n)) = (m, (I n))
eval1 (m, (B b)) = (m, (B b))
eval1 (m, L n) = (m, L n)
eval1 (m, Void) = (m, Void)
eval1 (m, (Add (I n) (I m))) = (m,(I (n+m)))
eval1 (m, (Add (I n) b)) = (m, ((Add (I n) (eval1 b)))
eval1 (m, (Add a b)) = (m, (Add (eval1 (m, a)) b))
eval1 (m, (Mul (I n) (I m))) = (m, (I (n*m)))
eval1 (m, (Mul (I n) b)) = (m, (Mul (I n) (eval1 (m, b))))
eval1 (m, (Mul a b)) = (m, (Mul (eval1 (m, a)) b))
eval1 (m, (Succ (Pred (I n)))) = (m, (I (n)))
eval1 (m, (Succ (I n))) = (m, (I (n+1)))
eval1 (m, (Succ a)) = (m, Succ (eval1 (m, a)))
eval1 (m, (Pred (Succ (I n)))) = (m, (I (n)))
eval1 (m, (Pred (I 0))) = (m, (I (0)))
eval1 (m, (Pred (I n))) = (m, (I (n-1)))
eval1 (m, (Pred a)) = (m, (Pred (eval1 (m, a))))
eval1 (m, (Not (B b))) = (m, (B (not b)))
eval1 (m, (Not b)) = (m, (Not (eval1 (m, b))))
eval1 (m, (And (B b) (B c))) = (m, (B (b&&c)))
eval1 (m, (And (B b) c)) = (m, (And (B b) (eval1 (m, c))))
eval1 (m, (And b c)) = (m, (And (eval1 (m, b)) c))
eval1 (m, (Or (B b) (B c))) = (m, (B (b||c)))
eval1 (m, (Or (B b) c)) = (m, (Or (B b) (eval1 (m, c))))
eval1 (m, (Or b c)) = (m, (Or (eval1 (m, b)) c))
eval1 (m, (Lt (I n) (I m))) = (m, (B (n<m)))
eval1 (m, (Lt (I b) c)) = (m, (Lt (I b) (eval1 (m, c))))
eval1 (m, (Lt b c)) = (m, (Lt (eval1 (m, b)) c))
eval1 (m, (Gt (I n) (I m))) = (m, (B (n>m)))
eval1 (m, (Gt (I b) c)) = (m, (Gt (I b) (eval1 (m, c))))
eval1 (m, (Gt b c)) = (m, (Gt (eval1 (m, b)) c))
eval1 (m, (Eq (I n) (I m))) = (m, (B (n==m)))
eval1 (m, (Eq (I b) c)) = (m, (Eq (I b) (eval1 (m, c))))
eval1 (m, (Eq b c)) = (m, (Eq (eval1 (m ,b)) c))
eval1 (m, (If (B b) x y))
    | b = (m, x)
    | otherwise = (m, y)
eval1 (m, (If b x y)) = (m, (If (eval1 (m,b) x y))
eval1 (m, (Let x (I n) c)) = (m, (subst c (x,(I n))))
eval1 (m, (Let x (B b) c)) = (m, (subst c (x,(B b))))
eval1 (m, (Let x (Fn y e) c)) = (m, (subst c (x,(Fn y e))))
eval1 (m, (Let x b c)) = (m, (Let x (eval1 (m, b)) c))
eval1 (m, (App (Fn x e) e2)) = (m, subst e (x, e2))
eval1 (m, (App e1 e2)) = (m, App (eval1 (m,e1)) e2)
eval1 (m, (Fn x e)) = (m, Fn x (eval1 (m, e)))
eval1 (m, Alloc e) = (((Memory.newAddress m),e):m, Void)
eval1 (m, Assig e1 e2) = (Memory.update (e1, e2) m, Void )
eval1 (m, Deref e) = (m, Memory.access e m)
eval1 (m, Seq e1 e2) = (m, Seq (eval1 (m, e1)) e2)
eval1 (m, Seq Void e2) = (m, e2) 
eval1 (m, While (B True) e2) = (m, Seq e2 (While (B True) e2))
eval1 (m, While (B False) e2) = (m, e2)
eval1 (m, While e1 e2) = While (eval1 (m, e1)) e2

evals :: Expr -> Expr
evals e = if (eval1 e) == e then e else evals (eval1 e)


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
    (Void) -> True
    otherwise -> False

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