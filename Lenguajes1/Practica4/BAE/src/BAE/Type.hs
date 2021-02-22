module BAE.Type where

  import qualified Data.List as List

  type Identifier = Int

  infix :->

  data Type = T Identifier 
            | Integer | Boolean
            | Type :-> Type deriving (Show,Eq)

  type Substitution = [(Identifier,Type)]

  vars :: Type -> [Identifier]
  vars (T t) = [t]
  vars (t1 :-> t2) = List.union (vars t1) (vars t2)
  vars t = []

  subst :: Type -> Substitution -> Type
  subst (T t) s = case s of
                  [] -> T t
                  ((x,t') : ss) -> if x == t 
                                   then t'
                                   else subst (T t) ss 
  subst (t1 :-> t2) s = (subst t1 s) :-> (subst t2 s)
  subst t s = t

  comp :: Substitution -> Substitution -> Substitution
  comp s1 s2 = List.union [(x, subst t s2) | (x,t) <- s1] [(x,t) | (x,t) <- s2, List.notElem x [y | (y, t) <- s1]]

  simpl :: Substitution -> Substitution
  simpl [] = []
  simpl ((i,t):xs) = case t of
                   (t1 :-> t2) -> (i,t):(simpl xs)
                   (T x) -> if i == x
                            then simpl xs
                            else (i,t):(simpl xs)
                   (t') -> (i,t):(simpl xs)