-- Ficha 4 -- 
import Data.Char 

digitAlpha :: String -> (String, String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isAlpha x = (x : ls, rs)
   where (ls, rs) = digitAlpha xs
digitAlpha (x:xs) | isDigit x = (ls, x : rs)
   where (ls, rs) = digitAlpha xs
digitAlpha (x:xs) = (ls, rs) 
   where (ls, rs) = digitAlpha xs 

nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0, 0, 0)
nzp (x:xs) | x < 0 = (1 + n, z, p)
   where (n,z,p) = nzp xs
nzp (x:xs) | x == 0 = (n, 1+z, p)
   where (n,z,p) = nzp xs
nzp (x:xs) | x > 0 = (n,z,1+p)
   where (n,z,p) = nzp xs

divMod' :: Integral a => a -> a -> (a,a)
divMod' 0 _ = (0,0)
divMod' _ 0 = (0,0)
divMod' a b | a - b >= 0 = (1 + q, r) 
   where (q, r) = divMod' (a-b) b 
divMod' a b | a - b < 0 = (0, a)

acumula :: Int -> [Int] -> Int 
acumula acc [] = acc 
acumula acc (x:xs) = acumula ( x + 10 * acc ) xs 

acumula' :: Int -> [Int] -> Int 
acumula' = foldl (\ acc x -> x + 10 * acc) 

fromDigits :: [Int] -> Int 
fromDigits = acumula' 0 

maxSumAcc :: (Num a, Ord a) => a -> [a] -> a 
maxSumAcc acc [] = acc 
maxSumAcc acc (x:xs) 
   | x < 0 = acc 
   | otherwise = maxSumAcc (x+acc) xs 

maxSumInit :: (Num a, Ord a) => [a] -> a 
maxSumInit = maxSumAcc 0 

fibAcc :: Int -> Int -> Int -> Int 
fibAcc acc1 _ 0 = acc1 
fibAcc acc1 acc2 n = fibAcc (acc1+acc2) acc1 (n-1)

fib :: Int -> Int 
fib 0 = 0 
fib 1 = 1 
fib n = fibAcc 1 0 (n-1)

toChr :: Int -> Char 
toChr = chr . (+ ord '0')

intToStrAcc :: String -> Int -> String 
intToStrAcc acc n | n < 10 = toChr n : acc 
intToStrAcc acc n =  intToStrAcc (toChr p  : acc) (n `div` 10)
   where p = n `mod` 10 

-- O argumento está no tipo Int para evitar conversões a partir do tipo Integer pela função fromIntegral. -- 

intToStr :: Int -> String 
intToStr = intToStrAcc [] 

aa :: [Int]
aa = [6,12,18]

bb :: [Int]
bb = [6,12,18]

cc :: [(,) Int Int]
cc = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

a :: [Int] 
a = [2^n | n <- [0..10]] 

b :: [(,) Int Int]
b = [(n,6-n) | n <- [1..5]]

c :: [[Int]]
c = [take n [1..5] | n <- [1..5]]

d :: [[Int]]
d = [replicate n 1 | n <- [1..5]]

e :: [Int]
e = [factorial n | n <- [1..6]]

factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n-1)
