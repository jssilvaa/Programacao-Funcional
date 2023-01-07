{-
TODO (T9+T10)
1. Def. BTrees: lista to BTree e Inverso DONE (Inorder, intra-order & post-order) 
2. BST - funções associadas / vantagens DONE  
3. Quicksort em BST's DONE
4. Multitrees e Ficheiros - find, path, etc. DONE
5. Expressões em árvores DONE
-}

data BTree a = V | N a (BTree a) (BTree a) deriving Show

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
-> São, para qq tipo de arvóres, as que descem o menor número de nodos até encontrar o número desejado
-> Se n for a altura da árvore, temos 2^n descendentes em cada nível
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

-- Supondo uma [a] ordenada -- 
fromListAux :: (Ord a) => Int -> [a] -> BTree a
fromListAux n [] = V
fromListAux n [x] = N x V V
fromListAux n l = N x left right
    where p = div n 2
          x = l !! p
          left = fromListAux p (take p l)
          right = fromListAux (n-p-1) (drop (p+1) l)

fromList1 :: (Ord a) => [a] -> BTree a
fromList1 l = fromListAux (length l) l

-- 2. Multi-trees 
data R a = Empty | Node a [R a] deriving Show
data SFich = Fich String
           | Dir String [SFich] deriving Show 

findMT :: (Eq a) => a -> R a -> Bool
findMT x (Node y l) = x == y || any (findMT x) l
findMT _ _ = False

findFich :: String -> SFich -> Bool
findFich s (Fich x) = s == x
findFich s (Dir x l) = s == x || any (findFich s) l

justJust :: [Maybe a] -> [Maybe a]
justJust xs = xs >>= \x -> case x of Just y -> [Just y]; _ -> []

pathFichAcc :: String -> String -> SFich  -> Maybe String 
pathFichAcc acc s (Fich x)  | (==) x s = Just (acc ++ "/" ++ x)
pathFichAcc acc s (Dir ndir l) = head(justJust(map (pathFichAcc (acc ++ "/" ++ ndir) s) l))
pathFichAcc _ _ _ = Nothing

pathFich :: String -> SFich -> Maybe String 
pathFich = pathFichAcc ""

sampleFich :: SFich 
sampleFich = Dir "bonito" [Fich "girissimo", Fich "caros_jovens", Dir "quentinho" [Fich "aqui_estou", Dir "naonao" [Fich "maisfundo"]]]

lengthDir :: SFich -> Int
lengthDir (Fich _ ) = 0
lengthDir (Dir _ l) = 1 + maximum (map lengthDir l)

longestDir :: [SFich] -> Int
longestDir l = maximum (map lengthDir l)

shortestDir :: [SFich] -> Int
shortestDir l = minimum (map lengthDir l)

-- 3. Expressões 
type Operador = Char 
data Exp = Const Int 
         | Op Operador Exp Exp 

evaluateExp :: Exp -> Int 
evaluateExp (Const x) = x 
evaluateExp (Op op e1 e2) = case op of '+' -> left + right 
                                       '-' -> left - right 
                                       '*' -> left * right  
   where left = evaluateExp e1 
         right = evaluateExp e2 

writeExp :: Exp -> String 
writeExp (Const x) = show x 
writeExp (Op op e1 e2) = "(" ++ left ++ " " ++ [op] ++ " " ++ right ++ ")"
    where left = writeExp e1  
          right = writeExp e2 

exExp :: Exp 
exExp = Op '*' (Op '+' (Op '+' (Op '+' (Op '+' (Const 5) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '-' (Const 5) (Const 2))) (Op '+' (Const 3) (Op '-' (Const 10) (Const 3))) 
