-- type inference algorithm for lambda terms

module TypeInf where
import LamGen -- includes generators for terms of given size


-- data Term a = V a | L (Term a) | A (Term a) (Term a) deriving (Eq,Show,Read)

-- type inference using unification with occursurs check

data TypeExpr = Var Int | Arr TypeExpr TypeExpr deriving (Eq,Show,Read)

type Env = [(Int,TypeExpr)]

-- unify with occurs check: ensures types are consistent 
unify :: TypeExpr->TypeExpr->Env->Maybe Env

unify t1 t2 vs = unify1 (deref t1 vs) (deref t2 vs) vs

unify1 :: TypeExpr->TypeExpr->Env->Maybe Env

unify1 (Var i) (Var j) vs  =  Just vs' where 
  vs' = if (i==j) then vs else (max i j,Var (min i j)):vs
unify1 (Var i) t vs = bind i t vs
unify1 t (Var j) vs = bind j t vs
unify1 (Arr x1 y1) (Arr x2 y2) vs = f mvs1 where
  mvs1 = unify x1 x2  vs
  f Nothing = Nothing
  f (Just vs1) = unify y1 y2 vs1
 
bind i t vs | occurs1 i t vs = Nothing
bind i t vs = Just ((i,t):vs)

deref (Var i) vs = deref1 i vs vs 
deref t _ = t
 
deref1 i ((j,t):vs) gvs = if i==j then deref t gvs else deref1 i vs gvs
deref1 i [] _ = Var i 

-- occursurs check
occurs i t vs = occurs1 i (deref t vs) vs

occurs1 i (Var j) vs = i==j
occurs1 i (Arr x y) vs = (occurs i x vs) || (occurs i y vs)  
   
-- inferred type
typeOf x = f (tof x (0,[], [])) where
  f Nothing = Nothing
  f (Just (_,t,es)) = Just (extractType t es)

-- returns (Just t) if typable, Nothing otherwise  
tof :: Term  -> (Int, [TypeExpr], Env) -> Maybe (Int, TypeExpr, Env)
  
tof (V k) (i,ts,es) = Just (i,ts!!k,es)
tof (L a) (i,ts,es)  = f m where
  m = tof a (i+1, ((Var i):ts), es)
  f Nothing = Nothing
  f (Just (j,t,es1)) = Just (j,(Arr (Var i) t),es1)
tof (A a b) (i,ts,es) = tof1 r1  where
  -- we ensure the two branches of an application agree on type
  r1 = tof a (i,ts,es)

  -- propagate Nothing up
  tof1 Nothing = Nothing
  tof1 (Just (i1,xy,es1))  = tof2 r2 where  
    r2 = tof b (i1,ts,es1)
    
    tof2 Nothing = Nothing
    tof2 (Just (i2,x,es2)) = f r where
      y = Var i2
      r = unify xy (Arr x y) es2
  
      f Nothing = Nothing
      f (Just es3) = Just (i2+1,y,es3)

-- follow variable references to obtain most general type   
extractType t es = et (deref t es) where
  et (Var i) = Var i
  et (Arr a b) = Arr (extractType a es) (extractType b es)

-- A220471: Number of closed simply typable lambda-terms of size n
-- with size 0 for the variables
-- 1, 2, 9, 40, 238, 1564, 11807, ...
genClosedTypable n = typed where 
  cterms = lambdaGen n
  typed = getTypes cterms (map typeOf cterms)
 
getTypes [] [] = []
getTypes (x:xs) (Just t:ts) = (x,t):getTypes xs ts 
getTypes (_:xs) (Nothing:ts) = getTypes xs ts

-- see LamTests for examples