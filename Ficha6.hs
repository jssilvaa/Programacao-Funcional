{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ficha6 where

-- 1 -- 
data BTree a = Empty
             | Node a (BTree a) (BTree a) deriving Show

--
sampleBT :: BTree Int
sampleBT = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))
--

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ x y) = 1 + max (altura x) (altura y)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ x y) = 1 + contaNodos x + contaNodos y

folhas :: BTree a -> Int
folhas Empty = 1
folhas (Node _ x y) = folhas x + folhas y

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 x = Empty
prune n (Node a x y) = Node a (prune m x) (prune m y)
    where m = n - 1

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
path (h:t) (Node a x y) = a : path t z
    where z = if h then y else x

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a x y) = Node a (mirror y) (mirror x)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a t1 t2) (Node b t3 t4) = Node (f a b) (zipWithBT f t1 t3) (zipWithBT f t2 t4)
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT (Node (x,y,z) t1 t2) = let (ls, ms, rs) = unzipBT t1
                                   (xs, ys , zs) = unzipBT t2
                               in (Node x ls xs, Node y ms ys, Node z rs zs)
unzipBT _ = (Empty, Empty, Empty)

-- 2 -- (NOTA: assumindo árvores de procura)

minimo :: Ord a => BTree a -> a 
minimo (Node e Empty _) = e 
minimo (Node _ l _) = minimo l 

semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node e Empty _) = Empty 
semMinimo (Node e l r) = Node e (semMinimo l) r 

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node e Empty _) = (e, Empty)
minSmin (Node e l r) = (m, Node e bt r)
    where (m, bt) = minSmin l 

remove :: Ord a => a -> BTree a -> BTree a 
remove _ Empty = Empty 
remove a (Node e l r) 
    | a == e = Node m l right
    | a < e = Node e (remove a l) r 
    | a > e = Node e l (remove a r)
   where (m,right) = minSmin r

-- 3 -- (novamente assumindo uma btree de procura, ord. por número)
type Aluno = (Numero, Nome, Regime, Classificacao)
type Numero = Int 
type Nome = String 
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Float | Rep | Faltou deriving Show 
type Turma = BTree Aluno 

inscNum :: Numero -> Turma -> Bool 
inscNum _ Empty = False 
inscNum x (Node (num,_,_,_) l r) = x == num || inscNum x z 
   where z = if x > num then r else l 

inscNome :: Nome -> Turma -> Bool 
inscNome _ Empty = False 
inscNome s (Node (_,nome,_,_) l r) = s == nome || inscNome s l || inscNome s r 

trabEst :: Turma -> [(Numero, Nome)] 
trabEst Empty = []
trabEst (Node (nome, num, TE, _) l r) = (nome, num) : (trabEst l ++ trabEst r)
trabEst (Node _ l r) = trabEst l ++ trabEst r 

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing 
nota x (Node (num,_,_,clas) l r) 
   | x == num = Just clas 
   | x > num = nota x r 
   | otherwise = nota x l  

percFaltas :: Turma -> Float 
percFaltas t = (percFaltas' t / fromIntegral (contaNodos t)) * 100 

percFaltas' :: Turma -> Float 
percFaltas' Empty = 0 
percFaltas' (Node (_,_,_,Faltou) l r ) = 1 + percFaltas' l + percFaltas' r 
percFaltas' (Node _ l r) = percFaltas' l + percFaltas' r 

sumNotas :: Turma -> Float
sumNotas Empty = 0 
sumNotas (Node (_,_,_, Aprov x) l r ) = x + sumNotas l + sumNotas r 
sumNotas (Node _ l r) = sumNotas l + sumNotas r 

mediaAprov :: Turma -> Float 
mediaAprov t = sumNotas t / fromIntegral (contaNodos t)

aprovAvAcc :: Turma -> (Float, Float)
aprovAvAcc Empty = (0,0)
aprovAvAcc (Node (_,_,_,Aprov _) l r) = (1 + ls1 + ls2, rs1 + rs2)
    where (ls1, rs1) = aprovAvAcc l 
          (ls2, rs2) = aprovAvAcc r
aprovAvAcc (Node (_,_,_,Rep) l r) = (ls1 + ls2, 1 + rs1 + rs2)
    where (ls1, rs1) = aprovAvAcc l 
          (ls2, rs2) = aprovAvAcc r
aprovAvAcc (Node _ l r) = (ls1 + ls2, rs1 + rs2)
    where (ls1, rs1) = aprovAvAcc l 
          (ls2, rs2) = aprovAvAcc r

aprovAv :: Turma -> Float 
aprovAv t = let (ap, rep) = aprovAvAcc t 
                aval = ap + rep 
            in ( ap / aval ) * 100