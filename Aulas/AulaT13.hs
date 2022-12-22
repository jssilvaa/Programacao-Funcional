module Aulas.AulaT13 where

{- Conteúdo Lecionado 
1. Revisões, essencialmente;
2. Aplicação dos conteúdos lecionados na resolução de um problema, que consiste 
em tentar colocar parênteses numa expressão matemática de modo a obter, possivelmente, 
um valor específico. 
-}

-- Problema: Colocar os parênteses numa expressão matemática de modo a sacar um valor x -- 
-- Exemplo: Colocar parênteses em 5 - 3 * 2 + 4 de modo a obter 3. (Sol: 5 - (3 * 2) + 4)

data Exp = Const Int 
           | Op Char Exp Exp 

instance Show Exp where 
   show :: Exp -> String 
   show (Const x) = show x 
   show (Op o e1 e2) = "(" ++ show e1 ++ [' ', o, ' '] ++ show e2 ++ ")"

calcula :: Exp -> Int 
calcula (Const x) = x 
calcula (Op o e1 e2) = case o of '+' -> calcula e1 + calcula e2 
                                 '-' -> calcula e1 - calcula e2 
                                 '*' -> calcula e1 * calcula e2 
                                 _ -> 0 

parte :: String -> [(Char, String, String)]
parte s = if (b == "") then []
          else (x, a, xs) : [ (p, a ++ (x:q), r) | (p,q,r) <- l]
             where
                (a, b) = span (\x -> x /= '+' && x /= '-' && x /= '*') s
                (x:xs) = b   
                l = parte xs 

geraAlt :: String -> [Exp]
geraAlt [] = []
geraAlt s = if null a then [Const (read s)]
            else [Op x s1 s2 | (x, e1, e2) <- a, s1 <- geraAlt e1, s2 <- geraAlt e2]
    where a = parte s          

solve :: String -> Int -> Maybe String 
solve [] _ = Nothing 
solve s x = if null p then Nothing else Just $ ((fst . head) p)
    where p = filter (\(_,y) -> x == y) [(show e, calcula e) | e <- geraAlt s]

