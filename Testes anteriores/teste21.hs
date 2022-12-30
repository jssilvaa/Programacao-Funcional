-- Teste 21 -- 
-- 1 -- 
(\\) :: Eq a => [a] -> [a] -> [a] 
(\\) x [] = x
(\\) [] _ = []
(\\) x (h:t) = (\\) (delete h x) t
    where delete :: Eq a => a -> [a] -> [a]
          delete _ []    = []
          delete x (h:t) | h == x = t 
                         | otherwise = h : delete x t


-- 2 -- 
type MSet a = [(a,Int)]
-- 2 a --
removeMSet :: Eq a => a -> MSet a -> MSet a 
removeMSet _ [] = []
removeMSet x ((a, num):t) 
    | x == a = if (num == 1) then t else (a, num - 1) : t 
    | otherwise = (a, num) : removeMSet x t 

-- 2 b -- 
calcula :: MSet a -> ([a], Int)
calcula = foldl(\(elems, total) (a, num) -> (a : elems, num + total)) ([], 0) 

-- 3 -- 
partes :: String -> Char -> [String]
partes [] _ = []
partes s x = left : partes (drop 1 right) x 
    where (left, right) = span (/= x) s

-- 4 --
data BTree a = Empty | Node a (BTree a) (BTree a)

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

-- 4 a -- 
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) 
    | x < e = Node e (remove x l) r
    | x > e = Node e l (remove x r)
    | x == e = 
        case (l,r) of 
            (Empty,r) -> r
            (l,Empty) -> l 
            (l,r) -> let (vmin, tmin) = min_Tree r in Node vmin l tmin

min_Tree :: Ord a => BTree a -> (a, BTree a)
min_Tree (Node e Empty r) = (e, r)
min_Tree (Node e l r)     = (vMin, Node e tMin r) 
    where (vMin, tMin) = min_Tree l

-- 4 b -- 
instance Show a => Show (BTree a) where
    show Empty = "*"
    show (Node e l r) = "(" ++ show l ++ " <-" ++ show e ++ "-> " ++ show r ++ ")"

-- 5 -- 
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (h:t) = sortOn f ys ++ [h] ++ sortOn f zs 
    where x = f h 
          ys = [y | y <- t, f y <= x]
          zs = [z | z <- t, f z > x ]

-- 6 -- 
data FileSystem = File Nome | Dir Nome [FileSystem] 
type Nome = String

fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
                 Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

-- 6 a -- 
fichs :: FileSystem -> [Nome]
fichs (File x) = [x]
fichs (Dir _ l) = concatMap fichs l

-- 6 b -- 
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f (x:xs) = case f x of Just x' -> x' : maybeMap f xs 
                                Nothing -> maybeMap f xs 
maybeMap _ _ = []


dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (File file) [] = Just [file]
dirFiles (Dir dir files) (h:t) 
   | dir == h = let names_list = maybeMap (`dirFiles` t) files in  
                    if   (null $ names_list) then Nothing
                    else Just $ concat names_list
   | otherwise = Nothing 
dirFiles _ _ = Nothing

-- 6 c -- 
listaFich :: FileSystem -> IO ()
listaFich fs = putStr "> " >> 
            getLine >>= (\str -> 
                case (dirFiles fs (partes str '/')) of 
                        Just files -> print files
                        Nothing    -> putStrLn "Não é uma diretoria.")          
