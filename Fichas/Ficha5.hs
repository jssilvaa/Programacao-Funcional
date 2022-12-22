-- Ficha 5 -- 
import qualified Data.Bifunctor

-- Funções de ordem superior -- 
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs)
   | f x = True
   | otherwise = any' f xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = []
                    | otherwise = x : dropWhile' f xs

span' :: (a -> Bool) -> [a] -> (,) [a] [a]
span' f (p:ps) | f p = (p : ls, rs)
   where (ls, rs) = span' f ps
span' f s = ([], s)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f x (y:ys) | f x y = ys
                    | otherwise = y : deleteBy f x ys
deleteBy _ _ _ = []

qSortAux :: Ord b => (a -> b) -> [a] -> (,) [a] [a]
qSortAux f (x:y:ys) | f y <= f x = (y : ls, rs)
   where (ls, rs) = qSortAux f (x:ys)
qSortAux f (x:z:zs) | f x < f z = (ls, z : rs)
   where (ls, rs) = qSortAux f (x:zs)
qSortAux f _ = ([],[])

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f (x:xs) = sortOn f ys ++ [x] ++ sortOn f zs
   where (ys, zs) = qSortAux f (x:xs)
sortOn _ _ = []

y = sortOn snd [(3,1),(1,2),(2,5)]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs
filter' _ _ = []


type Monomio = (Float, Int)
type Polinomio = [Monomio]

-- Polynomial Samples -- 
p1 :: Polinomio
p1 = [(1,0),(2,0),(3,1),(1,2),(4,2),(5,1)]
p2 :: Polinomio 
p2 = [(1,0), (1,1)]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter' (\(x,y) -> y == n)

conta :: Int -> Polinomio -> Int
conta n p = length $ selgrau n p

grauAcc :: Int -> Polinomio -> Int
grauAcc acc ((b,e):t) | acc < e = grauAcc e t
                      | otherwise = grauAcc acc t
grauAcc acc _ = acc

grau :: Polinomio -> Int
grau = grauAcc 0

deriv :: Polinomio -> Polinomio
deriv = map (\(b,e) -> (b * fromIntegral e,e-1)) . simp'
   where simp' = filter (\(x,y) -> y /= 0)

calcula :: Float -> Polinomio -> Float
calcula x p = sum $ map (\(b,e) -> b * x ^ fromIntegral e) p

simp :: Polinomio -> Polinomio
simp = filter' (\(x,y) -> x /= 0)

mult :: Monomio -> Polinomio -> Polinomio
mult (a,e) = map(Data.Bifunctor.bimap (a *) (e +))

ordena :: Polinomio -> Polinomio
ordena = sortOn snd

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):t) = (b + sum [fst m | m <- t, snd m == e], e) : normaliza [m | m <- t, snd m /= e]

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ p1 ++ p2

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (p:ps) q = normaliza (map (\(x,y) -> Data.Bifunctor.bimap (x *) (y +) p) q ++ produto ps q)

equiv :: Polinomio -> Polinomio -> Bool
equiv p q = elem False $ zipWith' (==) p q

type Mat a = [[a]]

upperTriangular :: Mat Int
upperTriangular = [[1,2,3,4], [0,5,6,7], [0,0,8,9],[0,0,0,10]]

nonSquare :: Mat Int
nonSquare = [[1,2,3],[4,5,6]]

-- Matrix Samples -- 
mat1 :: Mat Float
mat1 = [[-2,0,0],[-1,1,1],[1,4,2]]
mat2 :: Mat Float
mat2 = [[-0.5,0,0],[0.75,-1,0.5],[-1.25,2,-0.5]]
identidade :: Mat Int
identidade = [[1,0],[0,1]]
matEx :: Mat Int
matEx = [[1,2,3],[0,4,5],[0,0,6]]

dimOK :: Mat a -> Bool
dimOK (x:xs) = dimOKcheck (length x) xs

dimOKcheck :: Int -> Mat a -> Bool
dimOKcheck check (x:xs) | length x == check = dimOKcheck check xs
                        | otherwise = False
dimOKcheck _ _ = True

-- Alternativo -- 

dimOK' :: Mat a -> Bool
dimOK' (h:t) = all (\x -> length x == length h) t

-- Ou ainda -- 

dimOK'' :: Mat a -> Bool
dimOK'' (h:t) = all ((==) (length h) . length) t

dimMat :: Mat a -> (Int,Int)
dimMat [[]] = (0,0)
dimMat m = (length m, (length . head) m)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = (zipWith . zipWith) (+)

transpose :: Mat a -> Mat a
transpose x | null $ head x = []
transpose m = map head m : transpose (map (drop 1) m)

identity :: Mat a -> Mat a
identity m = [[m !! i | i <- [0..p-1]] !! j | j <- [0..q-1]]
   where (p,q) = dimMat m

transpose' :: Mat a -> Mat a
transpose' m = [[(m !! j) !! i | j <- [0..p-1]] | i <- [0..q-1]]
   where (p,q) = dimMat m

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [ [ sum (zipWith (*) (m1 !! i) [a !! j | a <- m2]) | j <- [0..c-1] ] | i <- [0..l-1]]
   where (l,_) = dimMat m1
         (_,c) = dimMat m2

-- Variamos a coluna (j), fixando a posição (i). Cada iteração dá a entrada para a posição (i,j)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup m = all (== 0) [ sum [ (m !! i) !! j | j <- [0..i-1] ] | i <- [0..l-1] ]
   where (l,_) = dimMat m

-- the use of real avoids an instance declaration of eq 
triSup' :: Real a => Mat a -> Bool
triSup' = snd . foldl (\(acc1,acc2) line -> (acc1+1, all (== 0) (take acc1 line) && acc2)) (0,True)

rotateLeft :: Mat a -> Mat a
rotateLeft m = [[a !! j | a <- m] | j <- [c-1,c-2..0]]
   where (_,c) = dimMat m

rotateRight :: Mat a -> Mat a
rotateRight m = [ [ (m !! i) !! j | i <- [l-1,l-2..0] ] | j <- [0..c-1] ]
   where (l,c) = dimMat m
