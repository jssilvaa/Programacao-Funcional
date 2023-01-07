-- Época Especial 2021 -- 
-- 1 -- 

-- a --
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:t) = y : posImpares t 

-- b -- 
isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] _ = True 
isPrefixOf _ [] = False 
isPrefixOf (x:xs) (h:t) = x == h && isPrefixOf xs t 

-- 2 -- 
type Mat a = [[a]]
sample = [[1,2,3,4], [0,5,6,7], [0,0,8,9]]

-- a -- 
zeros :: (Eq a, Num a )=> Mat a -> Int 
zeros = sum . map (sum . map (\x -> if x == 0 then 1 else 0)) 

-- b -- 
addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat = zipWith . zipWith $ (+)

-- c -- 
transpose :: Mat a -> Mat a 
transpose m = [ [l !! i | l <- m] | i <- [0..size]]
    where size = (flip (-) 1) . length . head $ m 

-- 3 -- 
data BTree a = Empty 
             | Node a (BTree a) (BTree a)

-- a --
replace :: Eq a => BTree a -> a -> a -> BTree a 
replace Empty _  _ = Empty 
replace (Node e l r) x new_x 
    | e == x    = Node new_x (replace l x new_x) (replace r x new_x)
    | otherwise = Node e (replace l x new_x) (replace r x new_x) 

-- b -- Ter cuidado com as definições de integer e int. Erros de comparações não definidas são de evitar. 
insere :: Int -> String -> BTree (Int, String) -> BTree (Int, String)
insere num nome Empty = Node (num, nome) Empty Empty
insere num nome (Node (tree_num, tree_nome) l r) 
   | num == tree_num = Node (num, nome) l r 
   | num > tree_num  = Node (tree_num, tree_nome) l (insere num nome r)
   | num < tree_num  = Node (tree_num, tree_nome) (insere num nome l) r

-- 4 -- 
data RTree a = R a [RTree a]
type Dictionary = [RTree (Char, Maybe String)]

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
            R ('r', Nothing) [
                R ('a',Just "Face") [
                    R ('l', Nothing) [
                        R ('h', Nothing) [
                            R ('o', Just "Palavrao") []]   
                    ], 
                    R ('s', Just "Plural de cara") [] ],
                R ('o',Just "Objeto dispendioso") [],
                R ('r',Nothing) [
                    R ('o',Just "Automovel, veiculo terrestre movido a combustivel que se desloca em quatro rodas.") [] ]
     ]  ]   ]   ]

-- a --
consulta :: String -> Dictionary -> Maybe String 
consulta _ [] = Nothing 
consulta [x] ((R (c, desc) l):t) = if (x == c) then desc else consulta [x] t  
consulta (x:xs) ((R (c, desc) l):t)
   | x == c    = consulta xs l 
   | otherwise = consulta (x:xs) t   

-- b --
palavras :: Dictionary -> [String]
palavras [] = []
palavras ((R (c, desc) l):t) =
    case desc of Nothing -> let lista = palavras l 
                            in map ((:) c) lista ++ palavras t
                 Just _  -> [c] : ((map ((:) c)( palavras l)) ++ palavras t)

-- c -- 
apresenta :: Dictionary -> IO ()
apresenta dict = do c <- getChar 
                    putStrLn ""
                    let words_list    = filter (\x -> head x == c) (palavras dict)
                    let words_entries = map (\w -> (w, consulta w dict)) words_list 
                    putStrLn $ unlines $ map show words_entries
