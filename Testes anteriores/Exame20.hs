-- Exame 20 -- 
import System.Random ( randomRIO )

-- 1 a -- 
inits :: [a] -> [[a]] 
inits [] = [[]]
inits l = inits (init l) ++ [l]

-- 1 b --
isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] _ = True 
isPrefixOf _ [] = False 
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

-- 2 --
data BTree a = Empty | Node a (BTree a) (BTree a)

-- 2 a -- 
folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node _ Empty Empty) = 1 
folhas (Node _ l r) = folhas l + folhas r 

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e _ _) = [e]
path (h:t) (Node e l r) = e : path t (if h then r else l)

-- 3 -- 
type Polinomio = [Coeficiente]
type Coeficiente = Float 

-- 3 a -- 
valor :: Polinomio -> Float -> Float
valor p x = foldr (\(g, c) acc -> acc + c * (x^g)) 0 $ zip [0..] p 

-- 3 b -- 
deriv  :: Polinomio -> Polinomio 
deriv = tail . map (uncurry (*)) . zip [0..] 

-- 3 c -- 
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = zipWith (+) (freeLoad p1) (freeLoad p2)
   where max_length = max (length p1) (length p2)
         freeLoad p = p ++ (replicate (max_length - length p) 0)

-- 4 a --
type Mat a = [[a]]

quebraLinha :: [Int] -> [a] -> Mat a  
quebraLinha _ [] = []
quebraLinha (x:xs) l = ls : quebraLinha xs rs
    where (ls, rs) = splitAt x l

-- 4 b -- 
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _      = []
fragmenta (x:xs) ys m = fragmentaCols ys (take x m) ++ fragmenta xs ys (drop x m) 

fragmentaCols :: [Int] -> Mat a -> [Mat a]
fragmentaCols [] _     = []
fragmentaCols (y:ys) m = map (take y) m : fragmentaCols ys (map (drop y) m)

-- 4 c -- 
gera1 :: Int -> (Int, Int) -> IO [Int]
gera1 0 _    = return []
gera1 n (a,b) = do p  <- randomRIO (a, b)
                   ps <- gera1 (n-1) (a,b)
                   return (p:ps)

gera :: (Int, Int) -> (Int,Int) -> IO (Mat Int)
gera (0,_) _     = return []
gera (n,m) (a,b) = do p  <- gera1 m (a,b)
                      ps <- gera (n-1, m) (a,b)
                      return (p : ps)

-- Alternativamente -- 

geraMat :: (Int, Int) -> (Int, Int) -> IO (Mat Int)
geraMat (x, y) (a, b) = sequence $ replicate x $ sequence $ replicate y $ randomRIO (a, b)
