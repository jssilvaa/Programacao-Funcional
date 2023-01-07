{-
TODO (T9+T10)
1. Def. BTrees: lista to BTree e Inverso DONE (Inorder, intra-order & post-order) 
2. BST - funções associadas / vantagens DONE  
3. Quicksort em BST's DONE
4. RTRees e Ficheiros - find, path, etc. DONE
5. Expressões em árvores DONE
-}

data BTree a = V | N a (BTree a) (BTree a)

height :: BTree a -> Int
height V = 0
height (N _ b1 b2) = 1 + max x y
   where x = height b1
         y = height b2

-- Dado uma BT não ordenada -- 
findB :: (Eq a) => a -> BTree a -> Bool
findB _ V = False
findB x (N n l r) = x == n || findB x l || findB x r

-- Lista -> BST 
tobst :: (Ord a) => [a] -> BTree a
tobst [] = V
tobst (h:t) = N h (tobst l) (tobst r)
   where l = [x | x <- t, x <= h]
         r = [x | x <- t, x > h]

{- 
-- Vantagens de usar árvores binárias de procura 
-> São, para qq. tipo de arvóres, as que descem o menor número de nodos até encontrar o número desejado
-> Se n for a altura da árvore, temos 2^(n-1) descendentes em cada nível
-> Se N for o número de nodos, 2^n - 1 = N, onde n é a altura da árvore, pelo que n = log_2(N+1) 
-> Sabendo o número de nodos, sabemos a altura da árvore e vice-versa 
-> São de longe mais eficientes do que fazer procuras (repetitivas) em listas ordenadas 
-}

-- Dada uma BST 
findBst :: (Ord a) => a -> BTree a -> Bool
findBst x (N y l r) | x == y = True
                    | x < y = findBst x l
                    | x > y = findBst x r
findBst _ _ = False

-- BST -> Lista (QuickSort)
tolst :: BTree a -> [a]
tolst (N h l r) = tolst l ++ [h] ++ tolst r
tolst _ = []

{-
Nota que para qualquer lista l do tipo l :: Ord a => [a], 
devemos ter (tobst . tolst) l = l (funções inversas)
-}

-- Supondo uma [a] ordenada -> BST -- 
fromListAux :: (Ord a) => Int -> [a] -> BTree a
fromListAux _ [] = V
fromListAux _ [x] = N x V V
fromListAux n l = N x left right
    where p = div n 2
          x = l !! p
          left = fromListAux p (take p l)
          right = fromListAux (n - (p+1)) (drop (p+1) l)

fromList1 :: (Ord a) => [a] -> BTree a
fromList1 l = fromListAux (length l) l

-- 2. Rose Trees & Sistemas de ficheiros 
data RTree a = Empty | R a [RTree a] 

data SFich = Fich String
           | Dir String [SFich] 

-- Procura um elemento numa Rose Tree -- 
-- any :: (a -> Bool) -> [a] -> Bool; any pega numa condição f :: (a -> Bool) e verifica se algum dos elementos em [a] satisfaz essa condição.
findRT :: (Eq a) => a -> RTree a -> Bool
findRT x (Node y l) = x == y || any (findRT x) l
findRT _ _  = False

-- Verifica se um ficheiro de nome 's' se encontra no sistema de ficheiros 'SFich' --
findFich :: String -> SFich -> Bool
findFich s (Fich x) = s == x
findFich s (Dir x l) = s == x || any (findFich s) l

-- Dado o nome de um ficheiro e um SFich, retorna (maybe) um caminho para esse ficheiro --

-- Filtra os Justs numa lista com Justs e Nothings 
justJust :: [Maybe a] -> [Maybe a]
justJust xs = xs >>= \x -> case x of Just y -> [Just y]; _ -> []

-- Acumula o caminho e retorna possivelmente o caminho para o ficheiro a procurar --
pathFichAcc :: String -> String -> SFich  -> Maybe String 
pathFichAcc acc s (Fich x)  | (==) x s = Just $ acc ++ "/" ++ x
pathFichAcc acc s (Dir ndir l) = head $ justJust $ map (pathFichAcc (acc ++ "/" ++ ndir) s) l 
pathFichAcc _ _ _ = Nothing

-- Função que chama o acumulador -- 
pathFich :: String -> SFich -> Maybe String 
pathFich = pathFichAcc ""

-- Sample (test purposes)
sampleFich :: SFich 
sampleFich = Dir "bonito" [Fich "girissimo", Fich "caros_jovens", Dir "quentinho" [Fich "aqui_estou", Dir "naonao" [Fich "maisfundo"]]]
--

-- Saca a "altura" de um filesystem --
lengthDir :: SFich -> Int
lengthDir (Fich _ ) = 0
lengthDir (Dir _ l) = 1 + maximum (map lengthDir l)

-- Dada uma lista de sistemas de ficheiros, saca aquele cujo comprimento é maior --
longestDir :: [SFich] -> Int
longestDir l = maximum (map lengthDir l)

-- Dada uma lista de sistemas de ficheiros, saca aquele cujo comprimento é menor --
shortestDir :: [SFich] -> Int
shortestDir l = minimum (map lengthDir l)

-- 3. Expressões 
type Operador = Char 
data Exp = Const Int 
         | Op Operador Exp Exp 

-- Avalia uma expressão Exp 
evaluateExp :: Exp -> Int 
evaluateExp (Const x) = x 
evaluateExp (Op op e1 e2) = case op of '+' -> left + right 
                                       '-' -> left - right 
                                       '*' -> left * right  
   where left = evaluateExp e1 
         right = evaluateExp e2 

-- Escreve uma expressão Exp (refazer isto mais tarde instanciado Exp como objeto de Show) --
writeExp :: Exp -> String 
writeExp (Const x) = show x 
writeExp (Op op e1 e2) = "(" ++ left ++ " " ++ [op] ++ " " ++ right ++ ")"
    where left = writeExp e1  
          right = writeExp e2 

-- Sample for testing purposes --
exExp :: Exp 
exExp = Op '*' (Op '+' (Op '+' (Op '+' (Op '+' (Const 5) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '+' (Const 3) (Op '-' (Const 10) (Const 3))) 
-- 
