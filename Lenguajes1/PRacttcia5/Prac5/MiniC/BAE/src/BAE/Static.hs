module BAE.Static where
  

  import qualified Data.List as List
  import BAE.Sintax
  import BAE.Type
  import BAE.Unifier
  type Ctxt = [(BAE.Sintax.Identifier , BAE.Type.Type)]
  type Constraint = [(BAE.Type.Type , BAE.Type.Type)]

    --Función que aplica una sustitución (de variables de tipo) a un contexto dado.

  subst :: Ctxt -> BAE.Type.Substitution-> Ctxt
  subst [] _ = []
  subst ((x,t):xs) l =  (x,BAE.Type.subst t l):(BAE.Static.subst xs l)
    -- Función que aplica una sustitución (de variables de tipo) a un contexto dado.
  find :: BAE.Sintax.Identifier -> Ctxt -> Maybe Type
  find _ [] = Nothing
  find x ((y, t):xs)
        | x == y = Just t
        | otherwise = find x xs

  freshAux :: Int -> [Type] -> Type
  freshAux n l
        | elem (T n) l = freshAux (n+1) l
        | otherwise = T n
--Dado un conjunto de variables de tipo, obtiene una variable de tipo fresca, es decir, que no aparece en este conjunto.
  fresh :: [BAE.Type.Type] -> BAE.Type.Type
  fresh l = freshAux 0 l
--Dada una expresión, infiere su tipo implementando las
--reglas descritas anteriormente. Devolviendo el contexto y el conjunto de
--restricciones donde es válido. Utiliza el conjunto de variables de tipo para
--crear variables de tipo frecas durante la ejecución.

  infer' :: ([BAE.Type.Type], BAE.Sintax.Expr) -> ([BAE.Type.Type], Ctxt, BAE.Type.Type, Constraint)
  
  infer' (d, (BAE.Sintax.V x)) = let t = fresh d
                                     d' = d `List.union` [t]
                                 in (d' , [(x, t)],  t,  [])
  infer' (d, (BAE.Sintax.I n)) = (d, [] , Integer, [])
  infer' (d, (BAE.Sintax.B b)) = (d, [] , Boolean, [])
  infer' (d, (BAE.Sintax.Add e1 e2)) = let  (a, h1, t1, r1) = infer' (d,e1)
                                            (b, h2, t2, r2) = infer' (a,e2)
                                            s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                            h = List.union h1 h2
                                            r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (b,h,Integer,r)
  infer' (d, (BAE.Sintax.Mul e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                           (b, h2, t2, r2) = infer' (a,e2)
                                           s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                           h = List.union h1 h2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (b,h,Integer,r)
  infer' (d, (BAE.Sintax.Succ e)) = let (c , h , t , re) = infer'(d,e)
                                        r = List.union re [(t,Integer)]
                                    in (c,h,Integer,r)
  infer' (d, (BAE.Sintax.Pred e)) = let (c , h , t , re) = infer'(d,e)
                                        r = List.union re [(t,Integer)]
                                    in (c,h,Integer,r)
  infer' (d, (BAE.Sintax.Not e)) = let (c , h , t , re) = infer'(d,e)
                                       r = List.union re [(t,Boolean)]
                                   in (c,h,Boolean,r)
  infer' (d, (BAE.Sintax.And e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                           (b, h2, t2, r2) = infer' (a,e2)
                                           s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                           h = List.union h1 h2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                       in (b,h,Boolean,r)
  infer' (d, (BAE.Sintax.Or e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                          (b, h2, t2, r2) = infer' (a,e2)
                                          s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                          h = List.union h1 h2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                       in (b,h,Boolean,r)
  infer' (d, (BAE.Sintax.Gt e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                          (b, h2, t2, r2) = infer' (a,e2)
                                          s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                          h = List.union h1 h2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (b,h,Boolean,r)
  infer' (d, (BAE.Sintax.Lt e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                          (b, h2, t2, r2) = infer' (a,e2)
                                          s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                          h = List.union h1 h2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (b,h,Boolean,r)
  infer' (d, (BAE.Sintax.Eq e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                          (b, h2, t2, r2) = infer' (a,e2)
                                          s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                          h = List.union h1 h2
                                          r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (b,h,Boolean,r)
  infer' (d, (BAE.Sintax.If f e1 e2)) = let (df, hf, tf, rf) = infer' (d,f)
                                            (a, h1, t1, r1) = infer' (df,e1)
                                            (b, h2, t2, r2) = infer' (a,e2)
                                            s12 = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                            sf1 = [(s1,s2)|(x,s1) <- h1, (y,s2) <- hf, x==y]
                                            sf2 = [(s1,s2)|(x,s1) <- hf, (y,s2) <- h2, x==y]
                                            h = hf `List.union` h1 `List.union` h2
                                            r = rf `List.union` r1 `List.union` r2 `List.union` s12 `List.union` sf1 `List.union` sf2 `List.union` [(t1,t2),(tf,Boolean)]
                                        in (b,h,t1,r)
  infer' (d, (BAE.Sintax.Let x e1 e2)) = let (a, h1, t1, r1) = infer' (d,e1)
                                             (b, h2, t2, r2) = infer' (a,e2)
                                             s = [(s1,s2)|(x,s1) <- h1, (y,s2) <- h2, x==y]
                                             h = List.union h1 h2
                                             r = r1 `List.union` r2 `List.union` s
                                          in case find x h2 of
                                              Just tx -> let r' = r `List.union` [(t1,tx)]
                                                             h' = h List.\\  [(x,tx)]
                                                         in (b,h',t2,r')
                                              Nothing -> let tx = fresh b
                                                             d' = b `List.union` [tx]
                                                             r' = r `List.union` [(t1,tx)]
                                                         in (d',h,t2,r')
  infer' (d, (BAE.Sintax.Fn x e)) = let (c, h, t, r) = infer' (d, e)
                                     in case find x h of
                                        Just t' -> (c,  h List.\\ [(x, t')] , t' :-> t, r)
                                        Nothing -> let t' = fresh c 
                                                       d' = c `List.union` [t']
                                                   in (d', h, t' :-> t, r)

  infer' (d, (BAE.Sintax.App e1 e2)) = let (a, h1, t1, r1) = infer' (d, e1)
                                           (b, h2, t2, r2) = infer' (a, e2)
                                           s = [(s1,s2) | (x,s1) <- h1, (y,s2) <- h2, x == y]
                                           z = fresh b
                                           d' = b `List.union` [z]
                                           h = h1 `List.union` h2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1 , t2 :-> z)]
                                        in (d', h, z, r)
  infer' (d ,(BAE.Sintax.Fn x e)) = let (d', h, t, r) = infer' (d, e)
                                      in case find x h of
                                        Just t' -> let h' = filter (\d -> x /= fst d) h
                                                       r' = r `List.union` [(t',t)]
                                                   in (d', h', t, r')  
                                        Nothing -> let z = fresh d' 
                                                       d'' = d' `List.union` [z]
                                                   in (d'', h, t, r)
--Dada una expresión, infiere su tipo devolviendo el contexto donde es válido.
  infer :: BAE.Sintax.Expr -> (Ctxt, BAE.Type.Type)
  infer e = let (_, h, t, r) = infer' ([], e)
                [umh] = BAE.Unifier.µ r 
            in  (BAE.Static.subst h umh, BAE.Type.subst t umh)

  
  fresh' :: Type -> [Type] -> Type
  fresh' (T n) vars = if List.elem (T n) vars
                      then fresh' (T $ succ n) vars
                      else (T n)

