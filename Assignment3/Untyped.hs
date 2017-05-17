import Parser (PTerm(..), Info, parse_)
import LamTests
import LamEval
import LamGen

data Binding = NameBind deriving (Show)

type Context = [(String, Binding)]

ctxLength :: Context -> Int
ctxLength = length

indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! n

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | x `elem` (map fst ctx) = pickFreshName ctx $ x ++ "'"
  | otherwise = ((x, NameBind) : ctx , x)

printTm :: Context -> PTerm -> String
printTm ctx t = case t of
  TmAbs _ x t1 -> let
      (ctx', x') = pickFreshName ctx x
    in "(\\" ++ x' ++ "." ++ (printTm ctx' t1) ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ (printTm ctx t1) ++ " " ++ printTm ctx t2 ++ ")"
  TmVar _ x n ->
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"

termShift d t = walk 0 t
  where
    walk c t = case t of
      TmVar fi x n -> if x >= c then
          TmVar fi (x + d) (n + d)
        else
          TmVar fi x (n + d)
      TmAbs fi x t1  -> TmAbs fi x $ walk (c + 1) t1
      TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

termSubst :: Int -> PTerm -> PTerm -> PTerm
termSubst j s t = walk 0 t
  where
    walk c t = case t of
      TmVar fi x n -> if x == j + c then
          termShift c s
        else
          TmVar fi x n
      TmAbs fi x t1  -> TmAbs fi x $ walk (c + 1) t1
      TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

j +-> s = termSubst j s

termSubstTop s t = termShift (-1) $ (0 +-> termShift 1 s) t

isVal :: Context -> PTerm -> Bool
isVal _ t = case t of
  TmAbs _ _ _ -> True
  _ -> False

eval1 :: Context -> PTerm -> Maybe PTerm
eval1 ctx t = case t of
  TmApp _ (TmAbs _ x t12) v2 | isVal ctx v2 -> Just $ termSubstTop v2 t12
  TmApp fi v1 t2 | isVal ctx v1 -> do
    t2' <- eval1 ctx t2
    return $ TmApp fi v1 t2'
  TmApp fi t1 t2 -> do
    t1' <-  eval1 ctx t1
    return $ TmApp fi t1' t2
  _ -> Nothing

eval :: Context -> PTerm -> PTerm
eval ctx t = case eval1 ctx t of
  Nothing -> t
  Just t' -> eval ctx t'

pretty s = case parse_ s of
  Left err -> print err
  Right t -> print $ printTm [] t

simplify :: String -> IO ()
simplify s = case parse_ s of
  Left err -> print err
  Right t  -> print $ printTm [] $ eval [] t

{-
data Term = V Int | L Term | A Term Term deriving (Eq,Show,Read)
data PTerm =
    TmVar Info Int Int
  | TmAbs Info String PTerm
  | TmApp Info PTerm PTerm
  deriving (Show)
 {=} ev (V x) = V x
ev (L x) = L (ev x) 
ev  (A x y) = ev1 (wh x) where
  ev1 (L a)   = ev (beta (L a) y)
  ev1 z = A (ev z) (ev y) 
-}
--data Term = V Int | L Term | A Term Term deriving (Eq,Show,Read)

conParse input = case parse_ input of 
   Left err -> Nothing
   Right b -> Just $ convertToTerm b  

convertToTerm :: PTerm -> Term
convertToTerm s = case s of  --need to change parse_ to other function which converts a String to Term
  TmVar _ i _ -> V i
  TmAbs _ _ t -> L (convertToTerm t)
  TmApp _ x y -> A (convertToTerm x) (convertToTerm y)  

mapValue :: (a->b)-> Maybe a -> Maybe b
mapValue g (Just a) = Just (g a)
mapValue g Nothing = Nothing


