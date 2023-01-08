-- Teste 22 -- 
import Data.List ( intercalate, nub, groupBy, sort)
import System.Random ( randomRIO )
import Control.Arrow ( (>>>) )

-- 1 --
_zip :: [a] -> [b] -> [(a,b)]
_zip (x:xs) (y:ys) = (x,y) : _zip xs ys 
_zip _ _ = []

-- 2 --
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:y:t) 
    | x <= y = x : preCrescente (y:t)
    | x > y = [x] 

-- 3 -- 
amplitude :: [Int] -> Int 
-- foldr :: (a -> b -> b) -> b -> t a -> t b
amplitude l = uncurry (-) . (foldr (\x (vMax, vMin) -> (max x vMax, min x vMin))) (head l, head l) $ l

-- 4 --
type Mat a = [[a]]
soma :: Num a => Mat a -> Mat a -> Mat a 
soma =  zipWith . zipWith $ (+)

-- 5 --
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome, [Telefone]) Agenda Agenda

ag = Nodo ("Daniel", [933212494, 98585397]) (Nodo ("Carlos", [986293417, 973782943]) Vazia Vazia) 
    (Nodo ("Frederico", [933764919, 253474929]) Vazia Vazia) 

instance Show Agenda where 
    show Vazia = ""
    show (Nodo (nome, tlfs) l r) = show l 
                        ++ nome ++ ":" ++ (replicate (10 - length nome) ' ') ++ (intercalate " / " $ map show tlfs) ++ "\n"
                        ++ show r 

-- 6 -- 
randomSel :: Int -> [a] -> IO [a]
randomSel 0 _ = return []
randomSel _ [] = return []
randomSel n l = do pos <- randomRIO (0, length l - 1)
                   let (ls, _:rs) = splitAt pos l
                   suffix <- randomSel (n-1) (ls ++ rs)
                   return ((l !! pos) : suffix) 

_randomSel :: Int -> [a] -> IO [a]
_randomSel 0 _  = return []
_randomSel _ [] = return [] 
_randomSel n l = randomRIO (0, length l - 1) 
            >>= (\pos -> 
                (l !! pos :) <$> _randomSel (n-1) (take pos l ++ drop (pos+1) l))

-- 7 -- 
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x (h:t) 
    | x == h = 0 : map (+1) (elemIndices x t) 
    | otherwise = map (+1) (elemIndices x t) 

organiza :: Eq a => [a] -> [(a, [Int])] 
organiza l = map (\x -> (x, elemIndices x l)) $ nub l

_organiza :: Eq a => [a] -> [(a, [Int])]
_organiza l = [(x, elemIndices x l) | x <- nub l]

-- special case, special circumstances -- 
__organiza :: String -> [(Char, [Int])]
__organiza = flip zip [0..]
            >>> sort 
            >>> groupBy (\x y -> fst x == fst y)
            >>> map (foldr (\(c, indx) (ch, indxs) -> (c, indx : indxs)) (' ', []))


-- 8 -- 
func :: [[Int]] -> [Int]
func [] = []
func (h:t) 
   | sum h > 10 = h ++ func t
   | otherwise = func t 

-- 9 --
data RTree a = R a [RTree a] deriving Show 
type Dictionary = [RTree (Char, Maybe String)] 

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
            R ('r', Nothing) [
                R ('a',Just "...") [
                    R ('s', Just "...") [] ],
                R ('o',Just "...") [],
                R ('r',Nothing) [
                    R ('o',Just "...") [] ]
     ]  ]   ]   ]

-- --
insere :: String -> String -> Dictionary -> Dictionary
insere [x] desc l = insereFim x desc l 
insere (x:xs) desc [] = [R (x, Nothing) (insere xs desc [])]
insere (x:xs) desc ((R (a,b) l):t) 
    | x == a = R (a,b) (insere xs desc l) : t
    | otherwise = R (a,b) l : insere (x:xs) desc t

insereFim :: Char -> String -> Dictionary -> Dictionary
insereFim x desc [] = [R (x, Just desc) []]
insereFim x desc ((R (a,b) l):t) 
    | x == a = (R (a, Just desc) l) : t
    | otherwise = (R (a, b) l) : insereFim x desc t 
