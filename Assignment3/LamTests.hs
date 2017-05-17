module LamTests where

import LamGen
import TypeInf
import LamEval

import System.CPUTime

-- testers for lambda term operations

-- tests  for term generation

countLams n = map length (map lambdaGen [0..n-1])

showLams = mapM_ print (lambdaGen 3) 

countLams1 = map length (map lambdaGen1 [0..9]) 

showLams1 = mapM_ print (lambdaGen1 4) 

-- helpers to show nicer terms

-- shows types when inference succeeds

tshow Nothing = "?" 
tshow (Just t) = tshow1 t

tshow1 (Var i) | i>=26 = "_"++(show i) 
tshow1 (Var i )| i<26 = [['A'..'Z']!!i]
tshow1 (Arr x y) = "("++(tshow1 x)++"->"++(tshow1 y)++")"

-- S, K, I combinators are well typed   
k_ = L (L (V 1))

s_ = L (L(L (A (A (V 2) (V 0)) (A (V 1) (V 0)))))

i_ = L (V 0)  

omega_ = (L (A (V 0) (V 0))) -- not typable

tk = tshow (typeOf k_)
ts = tshow (typeOf s_)

to = typeOf omega_ -- has no type


too =  typeOf (A omega_ omega_)

pptoo = pp (A omega_ omega_)

-- normalization loops on this
loop = ev (A omega_ omega_)

-- tests

-- counts closed simply typed terms of size n
countTypable n = map length (map genClosedTypable [0..n-1]) 

showTypable n = mapM_ print (map f (genClosedTypable n)) where
  f (x,t)=(x,tshow1 t)
  
benchmark mes f = do
  x<-getCPUTime
  print f
  y<-getCPUTime
  let time=(y-x) `div` 1000000000
  return (mes++" :time="++(show time))

-- measures how long it takes for closed terms
bmc n = benchmark 
   ("closed, size="++show n) 
   (length (lambdaGen n))

-- measures how long it takes for closed simply typed terms     
bmt n = benchmark 
   ("closed simply typed, size="++show n) 
   (length (genClosedTypable n))

-- "closed, size=8 :time=2006"
bm1 = bmc 8

-- "closed simply typed, size=8 :time=15188"
bm2 = bmt 8
   
 -- extra stuff

-- pretty printers
-- name gnerator
names1 = "xyz"++['a'..'w']
names2 = [0..]
names = (map (\x->[x]) names1) ++ (map ('x':) (map show names2))

-- showing a standard lambda term
lamshow (Vs i) = names !! i
lamshow (Ls x t) = ("(\\"++ (names !! x) ++ ".") ++ (lamshow t) ++ ")"
lamshow (As x y) = "(" ++ lamshow x ++ " " ++ lamshow y ++ ")"

-- same for deBruijn terms

lshow  = lamshow . b2s

pp dbTerm = putStrLn (lshow dbTerm)
-- for standard
sprint x = putStrLn (lamshow x)

-- for de Bruijn
bprint x = putStrLn (lamshow (b2s x))

-- shows that type is the same after evaluation

allTests n = mapM_ putStrLn (map (embelish . process) xts) where
  xts = genClosedTypable n
  process (x,t) = (x,t, y,just (typeOf y)) where y = ev x
  just (Just x) = x
  embelish (x,t,y,tt) = 
    lshow x ++ "::" ++ tshow1 t ++ "==>" ++ 
    lshow y ++ "::" ++ tshow1 tt

-- just ype this to see how it goes
go = allTests 3

s f g x = (f x) (g x)

k x y = x

skk = s k k 
