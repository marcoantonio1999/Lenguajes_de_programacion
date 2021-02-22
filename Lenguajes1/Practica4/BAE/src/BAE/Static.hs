module BAE.Static where

  import qualified Data.List as List
  import BAE.Sintax
  import BAE.Type
  import BAE.Unifier

  type Ctxt = [(BAE.Sintax.Identifier , BAE.Type.Type)]
  type Constraint = [(BAE.Type.Type , BAE.Type.Type)]

  subst :: Ctxt -> BAE.Type.Substitution -> Ctxt
  subst [] _ = []
  subst ((x, t) : cs) s = (x, BAE.Type.subst t s) : BAE.Static.subst cs s

  find :: BAE.Sintax.Identifier -> Ctxt -> Maybe Type
  find _ [] = Nothing
  find x ((y, t): cs) = if x == y 
                        then Just t
                        else find x cs

  fresh :: [BAE.Type.Type] -> BAE.Type.Type
  fresh vars = fresh' (T 0) vars

  infer' :: ([BAE.Type.Type], BAE.Sintax.Expr) -> ([BAE.Type.Type], Ctxt, BAE.Type.Type, Constraint)
  infer' (nv, (BAE.Sintax.V x)) = let t = fresh nv
                                      nv' = nv `List.union` [t]
                                  in (nv' , [(x, t)],  t,  [])
  infer' (nv, (BAE.Sintax.I n)) = (nv, [] , Integer, [])
  infer' (nv, (BAE.Sintax.B b)) = (nv, [] , Boolean, [])
  infer' (nv, (BAE.Sintax.Add e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                            (nv2, g2, t2, r2) = infer' (nv1,e2)
                                            s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                            g = List.union g1 g2
                                            r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                        in (nv2,g,Integer,r)
  infer' (nv, (BAE.Sintax.Mul e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                            (nv2, g2, t2, r2) = infer' (nv1,e2)
                                            s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                            g = List.union g1 g2
                                            r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                        in (nv2,g,Integer,r)
  infer' (nv, (BAE.Sintax.Succ e)) = let (nve , g , t , re) = infer'(nv,e)
                                         r = List.union re [(t,Integer)]
                                     in (nve,g,Integer,r)
  infer' (nv, (BAE.Sintax.Pred e)) = let (nve , g , t , re) = infer'(nv,e)
                                         r = List.union re [(t,Integer)]
                                     in (nve,g,Integer,r)
  infer' (nv, (BAE.Sintax.Not e)) = let (nve , g , t , re) = infer'(nv,e)
                                        r = List.union re [(t,Boolean)]
                                    in (nve,g,Boolean,r)
  infer' (nv, (BAE.Sintax.And e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                            (nv2, g2, t2, r2) = infer' (nv1,e2)
                                            s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                            g = List.union g1 g2
                                            r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                        in (nv2,g,Boolean,r)
  infer' (nv, (BAE.Sintax.Or e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                           (nv2, g2, t2, r2) = infer' (nv1,e2)
                                           s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                           g = List.union g1 g2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Boolean),(t2,Boolean)]
                                       in (nv2,g,Boolean,r)
  infer' (nv, (BAE.Sintax.Gt e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                           (nv2, g2, t2, r2) = infer' (nv1,e2)
                                           s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                           g = List.union g1 g2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (nv2,g,Boolean,r)
  infer' (nv, (BAE.Sintax.Lt e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                           (nv2, g2, t2, r2) = infer' (nv1,e2)
                                           s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                           g = List.union g1 g2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (nv2,g,Boolean,r)
  infer' (nv, (BAE.Sintax.Eq e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                           (nv2, g2, t2, r2) = infer' (nv1,e2)
                                           s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                           g = List.union g1 g2
                                           r = r1 `List.union` r2 `List.union` s `List.union` [(t1,Integer),(t2,Integer)]
                                       in (nv2,g,Boolean,r)
  infer' (nv, (BAE.Sintax.If f e1 e2)) = let (nvf, gf, tf, rf) = infer' (nv,f)
                                             (nv1, g1, t1, r1) = infer' (nvf,e1)
                                             (nv2, g2, t2, r2) = infer' (nv1,e2)
                                             s12 = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                             sf1 = [(s1,s2)|(x,s1) <- g1, (y,s2) <- gf, x==y]
                                             sf2 = [(s1,s2)|(x,s1) <- gf, (y,s2) <- g2, x==y]
                                             g = gf `List.union` g1 `List.union` g2
                                             r = rf `List.union` r1 `List.union` r2 `List.union` s12 `List.union` sf1 `List.union` sf2 `List.union` [(t1,t2),(tf,Boolean)]
                                        in (nv2,g,t1,r)
  infer' (nv, (BAE.Sintax.Let x e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv,e1)
                                              (nv2, g2, t2, r2) = infer' (nv1,e2)
                                              s = [(s1,s2)|(x,s1) <- g1, (y,s2) <- g2, x==y]
                                              g = List.union g1 g2
                                              r = r1 `List.union` r2 `List.union` s
                                          in case find x g2 of
                                              Just tx -> let r' = r `List.union` [(t1,tx)]
                                                             g' = g List.\\  [(x,tx)]
                                                         in (nv2,g',t2,r')
                                              Nothing -> let tx = fresh nv2
                                                             nv' = nv2 `List.union` [tx]
                                                             r' = r `List.union` [(t1,tx)]
                                                         in (nv',g,t2,r')
  infer' (nv, (BAE.Sintax.Fn x e)) = let (nve, g, t, r) = infer' (nv, e)
                                     in case find x g of
                                        Just t' -> (nve,  g List.\\ [(x, t')] , t' :-> t, r)
                                        Nothing -> let t' = fresh nve 
                                                       nv' = nve `List.union` [t']
                                                   in (nv', g, t' :-> t, r)

  infer' (nv, (BAE.Sintax.App e1 e2)) = let (nv1, g1, t1, r1) = infer' (nv, e1)
                                            (nv2, g2, t2, r2) = infer' (nv1, e2)
                                            s = [(s1,s2) | (x,s1) <- g1, (y,s2) <- g2, x == y]
                                            z = fresh nv2
                                            nv' = nv2 `List.union` [z]
                                            g = g1 `List.union` g2
                                            r = r1 `List.union` r2 `List.union` s `List.union` [(t1 , t2 :-> z)]
                                        in (nv', g, z, r)
  infer' (nv ,(BAE.Sintax.Fix x e)) = let (nv', g, t, r) = infer' (nv, e)
                                      in case find x g of
                                        Just t' -> let g' = filter (\d -> x /= fst d) g
                                                       r' = r `List.union` [(t',t)]
                                                   in (nv', g', t, r')  
                                        Nothing -> let z = fresh nv' 
                                                       nv'' = nv' `List.union` [z]
                                                   in (nv'', g, t, r)

  infer :: BAE.Sintax.Expr -> (Ctxt, BAE.Type.Type)
  infer e = let (_, g, t, r) = infer' ([], e)
                [umg] = BAE.Unifier.µ r 
            in  (BAE.Static.subst g umg, BAE.Type.subst t umg)

  
  fresh' :: Type -> [Type] -> Type
  fresh' (T n) vars = if List.elem (T n) vars
                      then fresh' (T $ succ n) vars
                      else (T n)




  --infer' ( [] , Let "x" (B True) (And (V "x" )( Let "x" ( I 10 ) (Eq ( I 0 ) ( Succ (V "x" ))))))