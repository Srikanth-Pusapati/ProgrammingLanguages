module LamGen where

-- lambda terms - standard form
data STerm = Vs Int | Ls Int STerm | As STerm STerm deriving (Eq,Show,Read)

-- de Bruijn  (assumed closed) to canonical lambda expressions
b2s :: Term -> STerm
b2s x = f x 0 [] where -- or, instead of [], list of free vars
  f :: Term -> Int -> [Int] -> STerm
  f (V i) _ vs  = Vs (at i vs)
  f (A a b) v vs = As x y where
    x = f a v vs
    y = f b v vs
  f (L a) v vs = Ls v y where
    y = f a (v+1) (v:vs)

at 0 (x:_) = x 
at i (_:xs) | i> 0 = at (i-1) xs 

-- canonical (closed) lambda expressions  to deBruijn
s2b :: STerm -> Term
s2b x = f x [] where
  f :: STerm -> [Int] -> Term
  f (Vs x) vs = V (at x vs)
  f (As x y) vs = A (f x vs) (f y vs)    
  f (Ls v y) vs = L a where a = f y (v:vs) 
  
isClosedB :: Term -> Bool
isClosedB t = f t 0 where
  f (V n) d = n < d
  f (L a) d = f a (d+1)
  f (A x y) d = f x d && f y d

-- borrowing boolean operation
isClosedS :: STerm -> Bool
isClosedS = isClosedB . s2b  



-- 2 generators for lambda terms of given size

-- de Bruijn terms
data Term = V Int | L Term | A Term Term deriving (Eq,Show,Read)

-- A220894: Number of closed lambda-terms of size n with size 0 for the variables
-- 0, 1, 3, 14, 82, 579, 4741, 43977,454283,5159441

lambdaGen n = lamGen n 0

lamGen 0 l =  map V [0..l-1]
lamGen n l | n>0 = 
  map L (lamGen (n-1) (l+1)) ++
  [A x y|k<-[0..n-1], x<-lamGen k l,y<-lamGen (n - 1 - k) l]

-- A135501 Number of closed lambda-terms of size n and size 1 for the variables.
-- this is done by having each A/2 contructor pay for the variable it induces
-- as for each n of them there will be n+1 variable
-- 0, 1, 2, 4, 13, 42, 139, 506, 1915, 7558

lambdaGen1 n = lamGen1 n 0

lamGen1 0 l =  map V [0..l-1]
lamGen1 n l | n>0 = 
  map L (lamGen1 (n-1) (l+1)) ++
  [A x y|k<-[0..n-2], x<-lamGen1 k l, y<-lamGen1 (n-2 - k) l]

-- see LamTests for examples