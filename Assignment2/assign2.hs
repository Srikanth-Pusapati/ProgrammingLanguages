-- standard terms
data STerm = Vs Int | Ls Int STerm | As STerm STerm deriving (Eq,Show,Read)

-- beta reduction of an application
beta :: STerm->STerm->STerm
beta (Ls p x) y = subst x 0 y 

-- subst a i b:  replace in a vars at depth i with shifted b
subst :: STerm->Int->STerm->STerm
subst (As a1 a2) i b = As (subst a1 i b) (subst a2 i b)
subst (Ls p a) i b = Ls p (subst a i b)
subst (Vs n) i b | n>i = Vs (n-1) 
subst (Vs n) i b | n==i = update i 0 b
subst (Vs n) i _ | n<i = Vs n

-- shifting indices in substituted term
update :: Int->Int->STerm->STerm
update i k (As a b) = As (update i k a) (update i k b)
update i k (Ls p a) = Ls p (update (i+1) k a)
update i k (Vs n) | n>=k  = Vs (n+1)
update i k (Vs n) | n < k = Vs n


-- weak head normal form
wh :: STerm->STerm
wh (Vs x) = Vs x 
wh (Ls p x) = Ls p x
wh (As x y) = wh1 (wh x) where
  wh1 (Ls p e) = wh (beta (Ls p e) y)
  wh1 z = As z y

-- evaluation by normal order reduction
ev :: STerm -> STerm
ev (Vs x) = Vs x
ev (Ls p x) = Ls p (ev x) 
ev  (As x y) = ev1 (wh x) where
  ev1 (Ls p a)   = ev (beta (Ls p a) y)
  ev1 z = As (ev z) (ev y)