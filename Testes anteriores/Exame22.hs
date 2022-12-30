-- Exame 22 -- 
import System.Random ( randomRIO )

-- 1 -- 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x 
   | n < 0 = []
   | otherwise = x : replicate' (n-1) x 

-- 2 -- 
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (h:t) xs 
   | h `elem` xs = h : intersect t xs

-- 3 -- 
data LTree a   = Tip a | Fork (LTree a) (LTree a)
data FTree a b = Leaf a| No b (FTree a b) (FTree a b)

conv :: LTree Int -> FTree Int Int   
conv (Tip x) = Leaf x 
conv (Fork l r) = No p (conv l) (conv r)
   where p = sumLTree l + sumLTree r 
         sumLTree :: LTree Int -> Int
         sumLTree (Tip x) = x 
         sumLTree (Fork l r) = sumLTree l + sumLTree r 

-- 4 -- 
type Mat a = [[a]]

triSup :: (Eq a, Num a) => Mat a -> Bool 
triSup = all (all (==0)) . map (\(x,xs) -> take x xs) . zip [0..]

-- 5 -- 
data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

instance Show SReais where
   show (AA x y)    = "]" ++ show x ++ "," ++ show y ++ "[" 
   show (AF x y)    = "]" ++ show x ++ "," ++ show y ++ "]"
   show (FA x y)    = "[" ++ show x ++ "," ++ show y ++ "[" 
   show (FF x y)    = "[" ++ show x ++ "," ++ show y ++ "]" 
   show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

tira :: Double -> SReais -> SReais 
tira x (AA a b) 
   | a < x && x < b = Uniao (AA a x) (AA x b)
   | otherwise = AA a b
tira x (AF a b)
   | a < x && x < b = Uniao (AA a x) (AF x b)
   | x == b = AA a b 
   | otherwise = AF a b
tira x (FA a b) 
   | a < x && x < b = Uniao (FA a x) (AA x b)
   | x == a = AA a b 
   | otherwise = FA a b
tira x (FF a b)
   | a < x && x < b = Uniao (FA a x) (AF x b)
   | x == a = AF a b 
   | x == b = FA a b 
   | otherwise = FF a b 
tira x (Uniao a b) = Uniao (tira x a) (tira x b)  

-- 6 --
func0 :: Float -> [(Float,Float)] -> [Float]
func0 x l = map snd (filter ((>x) . fst) l)

func :: Float -> [(Float,Float)] -> [Float]
func _ [] = []
func x ((a,b):t)
   | a > x = b : func x t 
   | otherwise = func x t 

-- 7 -- 
subseqSum :: [Int] -> Int -> Bool 
subseqSum [] _  = False 
subseqSum l x = any (== x) [sum (take n l) | n <- [1..(length l)]] || subseqSum (drop 1 l) x 

subseqSum' :: [Int] -> Int -> (Bool, Maybe [Int])
subseqSum' [] _ = (,) False Nothing 
subseqSum' l x = if (length p == 0) then subseqSum' (drop 1 l) x 
                                    else (True, Just q)
   where p = filter (== x) [sum(take n l) | n <- [1..(length l)]] 
         q = head [take n l | n <- [1..length l], sum(take n l) == x]


-- 8 -- 
gera :: Int -> (Int,Int) -> IO [Int]
gera 0 _ = return []
gera n (a,b) = do p <- randomRIO (a,b)
                  ps <- gera (n-1) (a,b)
                  return (p:ps)

jogo :: Int -> (Int, Int) -> IO ()
jogo n (a,b) = do l <- gera n (a,b)
                  putStr "Indique um número: \n> "
                  num' <- getLine
                  let num = read num'
                  let (encontrado, Just subSeq) = subseqSum' l num 
                  putStrLn ("Lista gerada: " ++ show l)
                  if encontrado then (putStrLn("Existe uma subsequência cuja soma é " ++ show num ++ "!")
                                   >> putStrLn("Essa subsequência corresponde a: " ++ show subSeq))
                                else putStrLn("Não existe nenhuma subsequência cuja soma é " ++ show num ++ ".")
