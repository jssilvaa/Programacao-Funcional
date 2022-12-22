{-# LANGUAGE InstanceSigs #-}
module Aulas.AulaT11 where 

{- 
Matéria Lecionada: 
1. Classes <-> uma classe é um corpo de objetos que partilham propriedades semelhantes
que se reflete numa série de funções que, embora diferentes, tem o mesmo intuito. 
2. Instâncias <-> Declarar uma estrutura de dados D como instância da classe A 
é definir como é que um número x de funções essenciais (que determinam as restantes) irão operar sobre essa estrutura, para que 
possam partilhar de um número y >= x maior de mais funções giras que podem atuar sobre esse tipo. 
3. O professor referiu classes de construtores (? mas não temos maturidade para isso, só pode estar a brincar) 
e ainda chegou a mencionar (assunto da próxima aula) I/O. 


Exemplos Utilizados
-> Classe Eq e classe Show. Criamos um tipo para números fracionários, data Frac = F Int Int. Ao F chamamos 
de construtor de tipo, F :: Int -> Int -> Frac. 
-> Definimos a igualdade entre números fracionários como a igualdade do produto cruzado (a desigualdade é simplesmente 
negar a igualdade, pelo que definir uma ou outra é indiferente, a outra fica automaticamente determinada pela negação da anterior). 
-> Definimos duas formas de mostrar um número fracionário, a maneira standard feita pelo ghci -> "F x y" (fazendo data Frac = F Int Int deriving Show)
e açúcar sintático como "x/y". Isto explica a maneira como vemos listas como [x,y,z] e não x : y : z : []. 
-> O mesmo foi feito para btrees, desta vez com o acréscimo de que o tipo da btree deve ser igualmente instância da 
classe em questão (Eq ou Show) (ou seja, se a BTree for do tipo BTree a, para que a BTree seja comparável é necessário que 
os seus elementos individuais do tipo a sejam comparáveis) pelo que temos impor essa restrição ao instanciar como classe. 
-> Isso faz-se com instance (Eq a) => Eq (BTree a) where e lê-se como "BTree a é instanciado na classe Eq na condição 
de a ser uma instância da classe Eq, onde (define-se de seguida como comparar BTrees de a). Ver exemplo no fim do ficheiro."
 -}


-- 1ª Parte -- 

{-
class Eq a where 
    (==) :: a -> a -> Bool 
    (/=) :: a -> a -> Bool 
    {-# MINIMAL (==) | (/=) #-}
    (==) a b = not ((/=) a b)  
    (/=) a b = not ((==) a b)  
-}

-- Samples -- 
a = F 2 4 :: Frac 
b = F 1 2 :: Frac 
c = F 2 5 :: Frac 

a1 = C 20 :: Temperature
b1 = K 293.15 :: Temperature
c1 = K 300 :: Temperature 
d1 = C 26.85 :: Temperature 

a2 = V :: BTree Int 
b2 = N 5 (N 3 (N 2 V V) (N 4 V V)) (N 3 (N 2 V V) (N 4 V V)) :: BTree Int 
c2 = N 5 (N 3 (N 2 V V) (N 4 V V)) (N 3 (N 2 V V) (N 4 V V)) :: BTree Int 
d2 = N 5 V (N 3 (N 2 V V) (N 4 V V)) :: BTree Int 
-- 

data Frac = F Int Int 

instance Eq Frac where 
    (==) :: Frac -> Frac -> Bool 
    (==) (F a b) (F c d) = a*d == b*c 

instance Show Frac where 
    show :: Frac -> String 
    --show (F x y) = "F " ++ show x ++ " " ++ show y
    show (F x y) = show x ++ "/" ++ show y 

data Temperature = C Double | K Double 

instance Eq Temperature where 
    (==) :: Temperature -> Temperature -> Bool 
    (==) (C x) (C y) = x == y 
    (==) (K x) (K y) = x == y 
    (==) (C x) (K y) = x + 273.15 == y 
    (==) (K x) (C y) = y + 273.15 == x 

instance Show Temperature where 
    show :: Temperature -> String 
    show (C x) = show x ++ " ºC"
    show (K x) = show x ++ " K"

-- 2ª Parte -- 
data BTree a = V | N a (BTree a) (BTree a)

instance (Eq a) => Eq (BTree a) where 
    (==) :: BTree a -> BTree a -> Bool 
    (==) V V = True  
    (==) (N x l1 r1) (N y l2 r2) = x == y && l1 == l2 && r1 == r2 
    (==) _ _ = False 

instance (Show a) => Show (BTree a) where 
    show :: BTree a -> String 
    --show V = ""
    --show (N e l r) = "(" ++ show l ++ ")" ++ " " ++ show e ++ " (" ++ show r ++ ")" 
    show V = ""
    show (N e l r) = "N " ++ show e ++ " (" ++ show l ++ ") (" ++ show r ++ ")"  

