module LamEval where
import LamGen
import TypeInf

-- standard terms
--data STerm = Vs Int | Ls Int STerm | As STerm STerm deriving (Eq,Show,Read)

-- de Bruijn terms: see LamGen
-- data Term = V Int | L Term | A Term Term deriving (Eq,Show,Read)

-- normalization of de Brujn terms (an "interpeter" for lambda calculus)

-- beta reduction of an application
beta :: Term->Term->Term
beta (L x) y = subst x 0 y 

-- subst a i b:  replace in a vars at depth i with shifted b
subst :: Term->Int->Term->Term
subst (A a1 a2) i b = A (subst a1 i b) (subst a2 i b) 
subst (L a) i b = L (subst a (i+1) b)
subst (V n) i b | n>i = V (n-1) 
subst (V n) i b | n==i = update i 0 b
subst (V n) i _ | n<i = V n

-- shifting indices in substituted term
update :: Int->Int->Term->Term

update i k (A a b) = A (update i k a) (update i k b)
update i k (L a) = L (update i (k+1) a)
update i k (V n) | n>=k  = V (n+i)
update i k (V n) | n < k = V n


-- weak head normal form
wh :: Term->Term
wh (V x) = V x 
wh (L x) = L x
wh (A x y) = wh1 (wh x) where
  wh1 (L e) =    wh (beta (L e) y)
  wh1 z = A z y

-- evaluation by normal order reduction
ev :: Term -> Term
ev (V x) = V x
ev (L x) = L (ev x) 
ev  (A x y) = ev1 (wh x) where
  ev1 (L a)   = ev (beta (L a) y)
  ev1 z = A (ev z) (ev y)

-- same for standard: we borrow from de Bruijn

evs :: STerm -> STerm
evs = b2s . ev. s2b

-- see LamTests for examples