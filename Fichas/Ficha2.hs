-- Ficha 2: Funções recursivas sobre listas --

import Data.Char ( ord , chr )
import GHC.Integer (floatFromInteger)
import GHC.Base (absentErr)

-- 1 --
funC :: [Int] -> [Int]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

funD :: [a] -> [a]
funD l = g [] l
g :: [a] -> [a] -> [a]
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- 2 --
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2 * h) : dobros t

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t)
  | c == h = 1 + numOcorre c t
  | otherwise = numOcorre c t

positivos :: [Int] -> Bool
positivos = foldr (\ h -> (&&) (h > 0)) True

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h>0 then h : soPos t else soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h < 0 then h + somaNeg t else somaNeg t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if length t < 3 then h:t else tresUlt t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros k ((x,y):t) = (k == x) || nosPrimeiros k t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos (x:y:t) = sumTriplos (soma3 x y : t)
   where soma3 (a,b,c) (a',b',c') = (a+a',b+b',c+c')

-- 3 -- 
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if ord h >= ord '0' && ord h <= ord '9' then h : soDigitos t else soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
  | ord h >= ord 'a' && ord h <= ord 'z' = 1 + minusculas t
  | otherwise = minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t)
  | ord h >= ord '0' && ord h <= ord '9' = (ord h - ord '0') : nums t
  | otherwise = nums t

-- 4 -- 

type Polinomio = [Monomio]
type Monomio = (Float, Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((a,e):t) = if n == e then 1 + conta n t else conta n t

grau :: Polinomio -> Int
grau [] = 0
grau ((a,e):t) = max e (grau t)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((a,e):t) = if n == e then (a,e) : selgrau n t else selgrau n t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,e):t) = (a * fromIntegral e, e - 1) : deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((a,e):t) = a * x^e + calcula x t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,e):t) = if a == 0 then simp t else (a,e) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,e1) ((b,e2):t) = (a*b,e1+e2) : mult (a,e1) t

qsort :: Polinomio -> Polinomio
qsort [] = []
qsort ((b,e):xs) = qsort ys ++ x' ++ qsort zs
    where ys = [(a,e1) | (a,e1) <- xs, e1 < e]
          zs = [(a,e1) | (a,e1) <- xs, e1 > e]
          x' = (b,e) : [(a,e1) | (a,e1) <- xs, e1 == e]

normaliza :: Polinomio -> Polinomio
normaliza p = normaliza' (qsort p)

normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' var@[(x,y)] = var
normaliza' ((b1,e1):(b2,e2):t) = if e1 == e2 then normaliza' ((b1+b2,e1):t) else (b1,e1) : normaliza' ((b2,e2):t)

soma :: Polinomio -> Polinomio -> Polinomio 
soma [] [] = []
soma x [] = x 
soma [] y = y 
soma x y = soma' (normaliza x) (normaliza y)

soma' :: Polinomio -> Polinomio -> Polinomio 
soma' x [] = x 
soma' [] y = y 
soma' pol1@((b1,e1):xs) pol2@((b2,e2):ys) 
   | e1 < e2 = (b1,e1) : soma' xs pol2  
   | e1 == e2 = (b1+b2, e2) : soma' xs ys 
   | e1 > e2 = (b2, e2) : soma' pol1 ys  

produto :: Polinomio -> Polinomio -> Polinomio 
produto _ [] = []
produto [] _ = []
produto p1 p2 = produto' (normaliza p1) (normaliza p2)

produto' :: Polinomio -> Polinomio -> Polinomio 
produto' (m:xs) p2 = normaliza (produto'' m p2 : produto' xs p2) <-- 

produto'' :: Monomio -> Polinomio -> Polinomio 
produto'' _ [] = []
produto'' m@(a1, e1) ((a2,e2):xs) = (a1*a2, e1*e2) : produto'' m xs
