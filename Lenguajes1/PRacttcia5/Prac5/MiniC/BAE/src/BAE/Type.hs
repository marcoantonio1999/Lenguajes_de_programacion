module BAE.Type where

    import qualified Data.List as List
    type Identifier = Int
    infix :->
    data Type = T Identifier
                | Integer | Boolean
                | Type :-> Type deriving(Eq,Show)

    type Substitution = [ ( Identifier , Type ) ]

    vars :: Type -> [Identifier]
    vars (T t) = [t]
    vars (t1 :-> t2) = List.union (vars t1) (vars t2)
    vars t = []

    substAux :: Type -> Substitution -> Type
    substAux (T x) [] = T x
    substAux (T x) ((y, (T z)):xs) 
        | x == y = T z
        | otherwise = substAux (T x) xs

    subst :: Type -> Substitution -> Type
    subst Integer _ = Integer
    subst Boolean _ = Boolean
    subst (T x) l = substAux (T x) l
    subst (t1 :-> t2) l = (subst t1 l) :-> (subst t2 l)

    comp :: Substitution -> Substitution -> Substitution
    comp [] l = l
    comp ((x, t):xs) (y:ys) = ((x, subst t (y:ys)):(comp xs (y:ys)))

    simpl :: Substitution -> Substitution
    simpl [] = []
    simpl ((x, T y):xs)
        | x == y = simpl xs
        | otherwise = (x, T y):(simpl xs)
    simpl (x:xs) = x:(simpl xs)