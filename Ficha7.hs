module Ficha7 where

-- 1 --
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const e) = e
calcula (Simetrico e) = - calcula e
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const e) = show e
infixa (Simetrico e) = "(" ++ "-" ++ infixa e ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " * " ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const e) = show e
posfixa (Simetrico e) = "-" ++ posfixa e
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

-- 2 -- 
data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e l) = e + foldr ((+) . soma) 0 l

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e l) = 1 + maximum (map altura l)

prune :: Int -> RTree a -> RTree a
prune _ (R e []) = R e []
prune 0 (R e _) = R e []
prune n (R e l) = R e (map (prune (n-1)) l)

mirror :: RTree a -> RTree a
mirror (R e l) = R e (reverse (map mirror l))

{- 
Travessias 

Tipos de Travessias: 
 Inorder -> Raiz -> Esquerda -> Direita         RED
 Intra-order -> Esquerda -> Raiz -> Direita     ERD
 Postorder -> Esquerda -> Direita -> Raiz       EDR

A travessia intra-order é importante em bst's uma vez que devolve uma lista ordenada. 

-}

inorder :: RTree a -> [a]
inorder (R e []) = [e]
inorder (R e l) = e : concatMap inorder l

-- não há maneira de definir uma travessia intra-order (um dos ramos pode ter um número ímpar de rosas) para rosetrees -- 

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e l) =  concatMap postorder l  ++ [e]

-- 3 --         

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork lt1 lt2) = ltSum lt1 + ltSum lt2

listaLt :: LTree a -> [a]
listaLt (Tip x) = [x]
listaLt (Fork lt1 lt2) = listaLt lt1 ++ listaLt lt2

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork lt1 lt2) = max x y
    where x = 1 + ltHeight lt1
          y = 1 + ltHeight lt2

-- 4 -- 

data BTree a = Empty | Node a (BTree a) (BTree a)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No e l r) = (Node e l1 l2, Fork r1 r2)
   where (l1, r1) = splitFTree l
         (l2, r2) = splitFTree r

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip b) = Just (Leaf b)
joinTrees (Node e l1 r1) (Fork l2 r2) = case (joinTrees l1 l2, joinTrees r1 r2) of
                                             (Just x, Just x') -> Just (No e x x')
                                             (_,_) -> Nothing
joinTrees _ _ = Nothing