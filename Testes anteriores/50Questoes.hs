{- 50 Questões 2022/2023 -}

-- 1 -- 
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y 
   | x > y = []
   | otherwise = x : enumFromTo' (x+1) y

-- 2 -- 
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' p q r 
   | p > r = []
   | otherwise = p : enumFromThenTo' (p + (q-p)) (q + (q-p)) r

-- 3 -- 
concatena :: [a] -> [a] -> [a]
concatena x [] = x
concatena [] x = x
concatena (x:xs) y = x : concatena xs y

-- 4 -- 
posicao :: [a] -> Int -> Maybe a
posicao l x | null l || x < 0 = Nothing
posicao (x:xs) 0 = Just x
posicao (x:xs) n = posicao xs (n-1)

-- 5 -- 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' x = last x : reverse' (init x )

-- 6 -- 
take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (y:ys)
   | x <= 0 = []
   | otherwise = y : take (x-1) ys

-- 7 -- 
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x (y:ys) 
   | x <= 0 = y 
   | otherwise = drop' (x-1) ys

-- 8 -- 
zip' :: [a] -> [b] -> [(,) a b]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

-- 9 -- 
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 10 
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x] 
intersperse' x (h:t) = h : x : intersperse' x t  

-- 11 -- 
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' x = p : group' (length p `drop` x) 
   where p = takeWhile (== head x) x 

-- 12 -- 
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- 13 -- 
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++  [l]

-- 14 --
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l@(h:t) = l : tails' t

-- 15 --  
heads'' :: [[a]] -> [a]
heads'' = map head

-- 16 --
total' :: [[a]] -> Int
total' = sum . map length

total'' :: [[a]] -> Int
total'' = foldr ((+) . length) 0

-- 17 -- 
fun :: [(a,b,c)] -> [(a,c)]
fun = map (\(x,y,z) -> (x,z))

-- 18 -- 
cola :: [(String, b, c)] -> String
cola = concatMap (\(x,y,z) -> x)

-- 19 --
idade' :: Int -> Int -> [(String, Int)] -> [String]
idade' year age = map fst . filter (\(s,y) -> year - y >= age)

-- 20 -- 
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = [ n^x | x <- [0..m-1] ]

-- 21 --
isPrime :: Int -> Bool
isPrime n = null [m | m <- [2..n'], mod n m == 0]
   where n' = ceiling(sqrt(fromIntegral n))

-- 22 --
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- 23 -- 
isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' x y = (==) x (drop r y)
   where r = length y - length x

-- 24 -- 
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) y = elem x y && isSubsequenceOf'' xs (dropWhile (/= x) y)

-- 25 -- 
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)
   | x == h = 0 : map (+1) (elemIndices' x t)
   | otherwise = map (+1) (elemIndices' x t)

-- 26 -- 
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t)
   | elem h t = nub' t
   | otherwise = h : nub' t

-- 27 --
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
   | x == h = t
   | otherwise = h : delete x t

-- 28 -- 
excepto :: Eq a => [a] -> [a] -> [a]
excepto l [] = l
excepto x [y] = delete y x
excepto x (y:ys) = delete y (excepto x ys)

-- 29 -- 
union :: Eq a => [a] -> [a] -> [a]
union x y = (++) x (filter (`notElem` x) y)

-- 30 --
intersect :: Eq a => [a] -> [a] -> [a]
intersect x y = filter (`elem` y) x

-- 31 -- 
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(h:t)
   | h < x = h : insert x t
   | h >= x =  x : l
   
-- 32 --
unwords' :: [String] -> String
unwords' = init . concatMap (flip (++) " ")

-- 33 --
unlines'' :: [String] -> String
unlines'' = concatMap (flip (++) "\n")

-- 34 -- 
pMaior :: Ord a => [a] -> Int 
pMaior [_] = 0
pMaior (h:t)
   | h < (t !! i) = i + 1
   | otherwise = 0
   where i = pMaior t

-- 35 -- 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' a x = if null p then Nothing else Just $ (snd . head) p
   where p = filter (\(x',_) -> x' == a) x

-- 36 --
prefixo :: Ord a => [a] -> [a]
prefixo (x:y:xs) = if x <= y then x : prefixo (y:xs) else [x]
prefixo [x] = [x]
prefixo _ = []

-- 37 -- 
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

-- 38 -- 
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) = if x == y then menor xs ys else x < y

-- 39 -- 
type MSet a = [(a,Int)]
elemMSet :: Eq a => a -> MSet a -> Bool
elemMSet z = any (\(x,y) -> (==) x z) 

-- 40 -- 
converteMSet :: MSet a -> [a]
converteMSet = concatMap (\(x,y) -> replicate y x) 

-- 41 -- 
insereMSet :: Eq a => a -> MSet a -> MSet a 
insereMSet z ms 
   | any (\(x,y) -> (== z) x) ms = [if z == x then (x,y+1) else (x,y) | (x,y) <- ms]
   | otherwise = (z,1) : ms

-- 42 -- 
removeMSet :: Eq a => a -> MSet a -> MSet a 
removeMSet z ms = filter (\(x,y) -> y /= 0) [if z == x then (x, y-1) else (x,y) | (x,y) <- ms]

-- 43 -- 
constroiMSet :: Ord a => [a] -> MSet a 
constroiMSet [] = []
constroiMSet x = (head p, r) : constroiMSet (drop r x)
    where p = [y | y <- x, y == head x]
          r = length p 

-- 44 -- 
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left l):t) = (l : ls, rs)
   where (ls, rs) = partitionEithers' t
partitionEithers' ((Right r):t) = (ls, r : rs)
   where (ls, rs) = partitionEithers' t
   
-- 45 --
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just x : xs) = x : catMaybes' xs  
catMaybes' (_:xs) = catMaybes' xs 

-- 46 -- 
data Movimento = Norte | Sul | Este | Oeste deriving (Show, Eq)

mov :: (Int,Int) -> [Movimento] 
mov (x,y) = p ++ q
   where p = if x >= 0 then replicate x Este else replicate (-x) Oeste
         q = if y >= 0 then replicate y Norte else replicate (-y) Sul
         
caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (a,b) (c,d) = mov (c-a, d-b)

-- 47 --
nmov :: Movimento -> [Movimento] -> Int
nmov _ [] = 0
nmov m (x:xs) = if x == m then 1 + nmov m xs else nmov m xs

hasloops :: [Movimento] -> Bool
hasloops [] = False
hasloops l
   | nmov Este l - nmov Oeste l == 0 && nmov Norte l - nmov Sul l == 0 = True
   | otherwise = hasloops $ init l

-- 48 --
type Ponto = (Float, Float)
data Rectangulo = Rect Ponto Ponto

contaRetangulo :: [Rectangulo] -> Int 
contaRetangulo = length . filter (\(Rect (x1,y1) (x2,y2)) -> abs (x2-x1) == abs (y2- y1))

-- 49 - 
areaTotal :: [Rectangulo] -> Float 
areaTotal = sum . map (\(Rect (x1,y1) (x2,y2)) -> abs ((x2-x1)*(y2-y1)))

-- 50 --
data Equipamento = Bom | Razoavel | Avariado

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Avariado : t) = naoReparar t
naoReparar (_:t) = 1 + naoReparar t
