{- 50 Questões 2022/2023 -}

-- 1 -- Constrói uma lista de inteiros compreendidos entre x e y.
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y 
   | x > y = []
   | otherwise = x : enumFromTo' (x+1) y

-- 2 -- Constrói uma lista de inteiros entre p e r em progressão aritmética de q-p.
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' p q r 
   | p > r = []
   | otherwise = p : enumFromThenTo' (p + (q-p)) (q + (q-p)) r

-- 3 -- Definição do operador (++) da Prelude.
concatena :: [a] -> [a] -> [a]
concatena x [] = x
concatena [] x = x
concatena (x:xs) y = x : concatena xs y

-- 4 -- Definição do operador (!!) da Prelude. 
posicao :: [a] -> Int -> Maybe a
posicao l x | null l || x < 0 = Nothing
posicao (x:xs) 0 = Just x
posicao (x:xs) n = posicao xs (n-1)

-- 5 -- Reverte uma lista (reverse na Prelude).
reverse' :: [a] -> [a]
reverse' [] = []
reverse' x = last x : reverse' (init x )

-- 6 -- take list op. from the Prelude library.
take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (y:ys)
   | x <= 0 = []
   | otherwise = y : take (x-1) ys

-- 7 -- drop list op. from the Prelude library.
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x (y:ys) 
   | x <= 0 = y 
   | otherwise = drop' (x-1) ys

-- 8 -- zip function from the Prelude library.
zip' :: [a] -> [b] -> [(,) a b]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

-- 9 -- replicate function from the Prelude library (sem guardas).
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 10 -- intersperse an element linking each element of the given list.
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x] 
intersperse' x (h:t) = h : x : intersperse' x t  

-- 11 -- group function from the Prelude library. 
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' x = p : group' (length p `drop` x) 
   where p = takeWhile (== head x) x 

-- 12 -- concat function from the Prelude library. 
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- 13 -- inits function (determines a list's set of prefixes).
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++  [l]

-- 14 -- tails function from the prelude library.
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l@(h:t) = l : tails' t

-- 15 -- receives a set of sets of a and outputs the set containing the first element of each set of a. 
heads'' :: [[a]] -> [a]
heads'' = map head

-- 16 -- Receives a list of lists and outputs the total element count.
total' :: [[a]] -> Int
total' = sum . map length

total'' :: [[a]] -> Int
total'' = foldr ((+) . length) 0

-- 17 -- Receives a list of triples and yields a list of tuples excluding the second component.
fun :: [(a,b,c)] -> [(a,c)]
fun = map (\(x,y,z) -> (x,z))

-- 18 -- Recebe uma lista de triplos com uma string na primeira posição, e dá output ao todo das strings concatenadas.
cola :: [(String, b, c)] -> String
cola = concatMap (\(x,y,z) -> x)

-- 19 -- Recebe um ano, uma idade, uma lista de (nomes,ano de nascimento) e dá output aos nomes das pessoas que já completaram ou vão completar "idade" no "ano" fornecido. 
idade' :: Int -> Int -> [(String, Int)] -> [String]
idade' year age = map fst . filter (\(s,y) -> year - y >= age)

-- 20 -- Constrói uma lista de potências n^0, .., n^(m-1).
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = [ n^x | x <- [0..m-1] ]

-- 21 -- Verifica se um número é primo.
isPrime :: Int -> Bool
isPrime n = null [m | m <- [2..n'], mod n m == 0]
   where n' = ceiling(sqrt(fromIntegral n))

-- 22 -- Verifica se a lista dada é um prefixo da outra.
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- 23 -- Verifica se a lista dada é um sufixo da outra.
isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' x y = (==) x (drop r y)
   where r = length y - length x

-- 24 -- Testa se a lista dada é uma subsequência da outra (subset na mesma ordem relativa).
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) y = elem x y && isSubsequenceOf'' xs (dropWhile (/= x) y)

-- 25 -- Enumera os índices das ocorrências de um elemento na lista.
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)
   | x == h = 0 : map (+1) (elemIndices' x t)
   | otherwise = map (+1) (elemIndices' x t)

-- 26 -- Calcula a mesma lista sem repetições.
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t)
   | elem h t = nub' t
   | otherwise = h : nub' t

-- 27 -- Retorna a lista depois de removida a primeira ocorrência do elemento fornecido.
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
   | x == h = t
   | otherwise = h : delete x t

-- 28 -- Retorna a lista depois de removidas as primeiras ocorrências de uma série de elementos de outra lista.
excepto :: Eq a => [a] -> [a] -> [a]
excepto l [] = l
excepto x [y] = delete y x
excepto x (y:ys) = delete y (excepto x ys)

-- 29 -- Calcula a união de duas listas, sem repetir os elementos da segunda que ocorrem na primeira.
union :: Eq a => [a] -> [a] -> [a]
union x y = (++) x (filter (`notElem` x) y)

-- 30 -- Remove os elementos da primeira lista que não pertencem à segunda.
intersect :: Eq a => [a] -> [a] -> [a]
intersect x y = filter (`elem` y) x

-- 31 -- Insere ordenadamente um elemento na lista. 
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(h:t)
   | h < x = h : insert x t
   | h >= x =  x : l
   
-- 32 -- Junta as strings de uma lista com um espaço.
unwords' :: [String] -> String
unwords' = init . concatMap (flip (++) " ")

-- 33 -- Junta as strings de uma lista com um "\newline".
unlines'' :: [String] -> String
unlines'' = concatMap (flip (++) "\n")

-- 34 -- Retorna o índice do maior elemento da lista.
pMaior :: Ord a => [a] -> Int 
pMaior [_] = 0
pMaior (h:t)
   | h < (t !! i) = i + 1
   | otherwise = 0
   where i = pMaior t

-- 35 -- Filtra o primeiro b cujo a satisfaz a condição dada (o enunciado da questão não bate certo com a definição).
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' a x = if null p then Nothing else Just $ (snd . head) p
   where p = filter (\(x',_) -> x' == a) x

-- 36 -- Calcula o maior prefixo crescente de uma lista. 
prefixo :: Ord a => [a] -> [a]
prefixo (x:y:xs) = if x <= y then x : prefixo (y:xs) else [x]
prefixo [x] = [x]
prefixo _ = []

-- 37 -- Insertion Sort.
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

-- 38 -- True se a primeira string for menor lexicograficamente que a primeira, false caso contrário.
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) = if x == y then menor xs ys else x < y

-- 39 -- Testa se o elemento pertence ao multiset.
type MSet a = [(a,Int)]

elemMSet :: Eq a => a -> MSet a -> Bool
elemMSet z = any (\(x,y) -> (==) x z) 

-- 40 -- Converte um multiset na lista dos seus elementos.
converteMSet :: MSet a -> [a]
converteMSet = concatMap (\(x,y) -> replicate y x) 

-- 41 -- Insere um elemento no multiset. 
insereMSet :: Eq a => a -> MSet a -> MSet a 
insereMSet z ms 
   | any (\(x,y) -> (== z) x) ms = [if z == x then (x,y+1) else (x,y) | (x,y) <- ms]
   | otherwise = (z,1) : ms

-- 42 -- Remove um elemento ao multiconjunto.
removeMSet :: Eq a => a -> MSet a -> MSet a 
removeMSet z ms = filter (\(x,y) -> y /= 0) [if z == x then (x, y-1) else (x,y) | (x,y) <- ms]

-- 43 -- Constrói um multiset a partir de uma lista ORDENADA de a's.
constroiMSet :: Ord a => [a] -> MSet a 
constroiMSet [] = []
constroiMSet x = (head p, r) : constroiMSet (drop r x)
    where p = [y | y <- x, y == head x]
          r = length p 

-- 44 -- Divide uma lista de Eithers em duas listas.
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left l):t) = (l : ls, rs)
   where (ls, rs) = partitionEithers' t
partitionEithers' ((Right r):t) = (ls, r : rs)
   where (ls, rs) = partitionEithers' t
   
-- 45 -- Coleciona os elementos a de uma lista de maybes.
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just x : xs) = x : catMaybes' xs  
catMaybes' (_:xs) = catMaybes' xs 

-- 46 -- 
data Movimento = Norte | Sul | Este | Oeste
               deriving (Show, Eq)
               
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
