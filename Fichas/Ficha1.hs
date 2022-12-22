-- Ficha 1: Funções não recursivas 

import Data.Char ( toUpper )

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x*t)) / t
    where t = 10^n

-- 1 --
perimetro :: (Ord p, Floating p) => p -> p
perimetro r | r < 0 = 0
perimetro r | r >= 0 = 2*pi*r

type CPoint = (Double, Double)
distance :: Floating a => (a, a) -> (a, a) -> a
distance (a,b) (c,d) = sqrt ((a-c)*(a-c) + (b-d)*(b-d))

--type Point = (Int, Int)
--primUlt :: [a] -> (a, a)
--primUlt (xs) = (head xs, last xs) 

multiplo :: Integral a => a -> a -> Bool
multiplo m n | mod m n == 0 = True
multiplo m n = False

truncaImpar :: [a] -> [a]
truncaImpar xs | even (length xs) = xs
truncaImpar xs = tail xs

max2 :: Integral a => a -> a -> a
max2 x y = if x > y then x else y

max3 :: Integral a => a -> a -> a -> a
max3 x y z = max2 (max2 x y) z

-- 2 --
nRaizes :: (Ord a, Num p, Num a) => a -> a -> a -> p
nRaizes a b c
  | d == 0 = 1
  | d > 0 = 2
  | otherwise = 0
  where
      d = b ^ 2 - 4 * a * c

raizes :: (Ord a, Floating a) => a -> a -> a -> [a]
raizes a b c
  | nRaizes a b c == 0 = []
  | r1 == r2 = [r1]
  | otherwise = [r1, r2]
  where
      d = b ^ 2 - 4 * a * c
      r1 = 1 / (2 * a) * (sqrt d - b)
      r2 = 1 / (2 * a) * ((- b) - sqrt d)


-- 3 --
data Hora = H Int Int deriving (Show,Eq)

valida :: (Ord a, Num a) => (a, a) -> Bool
valida (h, m) = (0 <= h && h < 24) && (0 <= m && m < 60)

compara :: (Ord a, Num a) => (a,a) -> (a,a) -> (a,a)
compara (h1,m1) (h2,m2)
  | not (valida (h1,m1) && valida (h2,m2)) = (-1, -1)
  | h1 < h2 = (h2, m2)
  | h1 > h2 = (h1, m1)
  | m1 < m2 = (h2, m2)
  | otherwise = (h1,m1)

htomin :: Num a => (a,a) -> a
htomin (h,m) = 60*h + m

mintoh :: Double -> Double
mintoh m = truncate' (m/60) 3


intervalo :: (Ord a, Num a) => (a,a) -> (a,a) -> a
intervalo (h1,m1) (h2,m2) = htomin (x,y)
  where
    x = if h1>h2 then h1-h2 else h2-h1
    y = if m1>m2 then m1-m2 else m2-m1

addhour :: Integral a => (a,a) -> a -> (a,a)
addhour (h,m) x = (h', m')
  where
    h' = if (x+m) >= 60 then (h + (x+m) `div` 60) `mod` 24 else h
    m' = mod (x+m) 60


checks :: Hora -> Bool
checks (H h m) = (0 <= h && h < 24) && (0 <= m && m < 60)


tComparison :: Hora -> Hora -> Hora
tComparison (H h1 m1) (H h2 m2)
  | checks (H h1 m1) && checks (H h2 m2) = H h m
  | otherwise = H (-1) (-1)
       where
         m | h1 == h2 = max m1 m2
           | h1 > h2 = m1
           | otherwise = m2
         h = max h1 h2

strcmp :: String -> String -> Bool 
strcmp s1 s2 = case (s1, s2) of 
   ([],[]) -> True
   (s1:ss1,s2:ss2)
     | toUpper s1 == toUpper s2 -> strcmp ss1 ss2 
     | otherwise -> False 
   _ -> False 
