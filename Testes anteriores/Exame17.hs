-- Exame 17 -- 

-- 1 -- 
-- a -- 
_unlines :: [String] -> String 
_unlines [] = []
_unlines [x] = x
_unlines (h:t) = h ++ "\n" ++ _unlines t 

__unlines :: [String] -> String 
__unlines = foldl(\acc h -> acc ++ (if (not $ null acc) then (flip (:) h '\n') else h)) []

-- b --
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) x []  = x 
(\\\) l (h:t) = (\\\) (delete h l) t 
    where 
        delete :: Eq a => a -> [a] -> [a]
        delete _ [] = []
        delete x (h:t)
            | x == h = t 
            | otherwise = h : delete x t 

-- 2 -- 
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a 

-- a -- 
primeiro :: Seq a -> a 
primeiro (Inicio x _) = x 
primeiro (Fim Nil x)  = x 
primeiro (Fim _seq x) = primeiro _seq

-- b --
semUltimo :: Seq a -> Seq a 
semUltimo (Inicio a Nil) = Nil 
semUltimo (Inicio a s)   = Inicio a (semUltimo s)
semUltimo (Fim Nil _)    = Nil 
semUltimo (Fim s _)      = s 

-- 3 -- 
data BTree a = Empty 
              | Node a (BTree a) (BTree a)

-- a -- 
prune :: Int -> BTree a -> BTree a 
prune 0 _ = Empty 
prune _ Empty = Empty 
prune n (Node e l r) = let n' = n - 1 in 
                       Node e (prune n' l) (prune n' r)

-- b --
semMinimo :: Ord a => BTree a -> BTree a 
semMinimo (Node _ Empty r) = r 
semMinimo (Node e l r)     = Node e (semMinimo l) r

-- 4 -- 
type Tabuleiro = [String]

tab = ["..R.",
       "R...",
       "...R",
       ".R.."]

-- a --
posicoes :: Tabuleiro -> [(Int, Int)]
posicoes = flip zip [0..] . map (length . takeWhile (/= 'R')) 

-- b --
valido :: Tabuleiro -> Bool 
valido [x] = True 
valido x = let ((c, l) : t) = posicoes x in 
           all (\(c', l') -> c /= c' && l /= l' && l + c' /= c + l') t && valido (tail x) 

-- c --
bemFormado :: Int -> Tabuleiro -> Bool 
bemFormado n = and . map ((== (1, n - 1)) . foldr(\x (r, p) -> 
                                                case x of 
                                                    'R' -> (1 + r, p)
                                                    '.' -> (r, 1 + p)
                                                    _   -> (r, p)) (0,0)
                                                    ) 

