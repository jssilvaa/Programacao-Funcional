{- 50 Questões -}

-- 1 -- Constrói a lista de inteiros compreendidos entre x e y 
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x > y = []
enumFromTo' x y = x : enumFromTo' (x+1) y

enumFromTo'' :: Int -> Int -> [Int]
enumFromTo'' x y = [x..y]

-- 2 -- Constrói a lista de inteiros entre p e r em progressão de q-p 
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' p q r | p > r = []
enumFromThenTo' p q r = p : enumFromThenTo' (p + (q-p)) (q + (q-p)) r

-- 3 -- Definição do operador (++) da Prelude 
concatena :: [a] -> [a] -> [a]
concatena x [] = x
concatena [] x = x
concatena (x:xs) y = x : concatena xs y

-- 4 -- Definição do operador (!!) da Prelude 
posicao :: [a] -> Int -> Maybe a
posicao [] _ = Nothing
posicao l x | x < 0 = Nothing
posicao (x:xs) 0 = Just x
posicao (x:xs) n = posicao xs (n-1)

-- 5 -- Reverte uma lista (reverse na Prelude)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' x = last x : reverse' (init x )

-- 6 -- take list op. from the Prelude library
take' :: Int -> [a] -> [a]
take' _ [] = []
take' x _ | x <= 0 = []
take' x (y:ys) = y : take (x-1) ys

-- 7 -- drop list op. from the Prelude library 
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x y | x <= 0 = y
drop' x (y:ys) = drop' (x-1) ys

-- 8 -- zip function from the Prelude library 
zip' :: [a] -> [b] -> [(,) a b]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

-- 9 -- replicate function from the Prelude library
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n x = x : replicar (n-1) x

-- 10 -- group function from the Prelude library (with aux. function)
agrupa :: Eq a => [a] -> [[a]]
agrupa [] = []
agrupa [x] = [[x]]
agrupa (x:xs)
   | x == head xs = let y = x : aux xs
                    in y : agrupa (drop (length  y - 1) xs)
   | otherwise = [x] : agrupa xs

aux :: Eq a => [a] -> [a]
aux [] = []
aux [x] = [x]
aux (x:xs)
   | x == head xs = x : aux xs
   | otherwise = [x]

-- 10 -- group function from the Prelude library (simpler version)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs)
   | elem x $ head r = ( x : head r ) : tail r
   | otherwise = [x] : r
   where r = group' xs

-- 11 -- concat function from the Prelude library 
concat' :: [[a]] -> [a]
concat' = foldr (++) [] -- use concat, so we know it is defined as in the prelude

-- 12 -- inits function (determines a list's set of prefixes)
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++  [l]

-- 13 -- tails function from the prelude library
tails :: [a] -> [[a]]
tails [] = [[]]
tails l@(h:t) = l : tails t

tails' :: [a] -> [[a]]
tails' [] = [[]]  -- avoids empty list exception. the upward way is a more efficient and secure way of executing this process.
tails' l = l : tails (tail l)

-- 14 -- receives a set of sets of a and outputs the set containing the first element of each set of a 
heads' :: [[a]] -> [a]
heads' [] = []
heads' (x:t) = take 1 x ++ heads' t -- It isn't possible to use the head function here. Why? Perhaps due to the possibility of catching an empty list within the greater list?

heads'' :: [[a]] -> [a]
heads'' = map head

heads''' :: [[a]] -> [a]
heads''' = concat . map (take 1) -- the very definition of concatMap -> map a certain application over a foldable object, and the concatenate the resulting lists within the list.

-- 16 -- Receives a list of lists and outputs the total element count
total :: [[a]] -> Int
total = sum . map length

total' :: [[a]] -> Int
total' [] = 0
total' (h:t) = length h + total t

total'' :: [[a]] -> Int
total'' = foldr ((+) . length) 0

-- 17 -- Receives a list of triples and yields a list of tuples with the first and last components
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

foo' :: [(a,b,c)] -> [(a,c)]
foo' = map (\(x,y,z) -> (x,z))

-- 18 -- Recebe uma lista de triplos com uma string na primeira posição, e dá output ao todo das strings concatenadas.
cola :: [(String, b, c)] -> String
cola [] = []
cola ((x,y,z):t) = x ++ cola t

cola' :: [(String, b, c)] -> String
cola' = concatMap (\(x,y,z) -> x)

-- 19 -- Recebe um ano, uma idade, uma lista de (nomes,ano de nascimento) e dá output aos nomes das pessoas que já completaram ou vão completar "idade" no "ano" fornecido. 
idade :: Int -> Int -> [(String, Int)] -> [String]
idade _ _ [] = []
idade y i ((nm, yy):t)
   | y - yy >= i = nm : idade y i t
   | otherwise = idade y i t

idade' :: Int -> Int -> [(String, Int)] -> [String]
idade' year age = map fst . filter (\(s,y) -> year - y >= age)

-- 20 -- Constrói uma lista de potências n^0, .., n^(m-1)
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = []
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n ^ (m-1)]

powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = [ n^x | x <- [0..m-1] ]

-- 21 -- Verifica se um número é primo
isPrime :: Int -> Bool
isPrime n = let n' = fromIntegral n
            in null ([m | m <- [2..n], 2 <= m && fromIntegral m <= sqrt n' && mod n m == 0])

isPrime' :: Int -> Bool
isPrime' n = null [m | m <- [2..n'], mod n m == 0]
   where n' = ceiling(sqrt(fromIntegral n))

-- 22 -- Verifica se a lista dada é um prefixo da outra
isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

-- 23 -- Verifica se a lista dada é um sufixo da outra
isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' x y = (==) x (drop r y)
   where r = length y - length x

-- 24 -- Testa se a lista dada é uma subsequência da outra (subset na mesma ordem relativa)
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys)
   | x == y = isSubsequenceOf' xs ys
   | otherwise = isSubsequenceOf' (x:xs) ys

isSubsequenceOf'' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf'' [] _ = True
isSubsequenceOf'' _ [] = False
isSubsequenceOf'' (x:xs) y = isSubsequenceOf'' xs $ dropWhile (/= x) y

{-splitAt' :: Eq a => a -> [a] -> [[a]]
splitAt' _ [] = []
splitAt' x [y] = if x == y then [[]] else [[y]]
splitAt' x (y:ys)
   | x == y = [] : r
   | otherwise = (y : head r) : tail r
   where r = splitAt' x ys
-}

-- 25 -- Enumera os índices das ocorrências de um elemento na lista
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l
    | elem x l = elemAux x l 0
    | otherwise = []

elemAux :: Eq a => a -> [a] -> Int -> [Int]
elemAux _ [] _ = []
elemAux x (h:t) i
   | x == h = i : elemAux x t (i+1)
   | otherwise = elemAux x t (i+1)

elemIndices'' :: Eq a => a -> [a] -> [Int]
elemIndices'' _ [] = []
elemIndices'' x (h:t)
   | x == h = 0 : map (+1) (elemIndices'' x t)
   | otherwise = map (+1) (elemIndices'' x t)

-- 26 -- Calcula a mesma lista sem repetições
nub'' :: Eq a => [a] -> [a]
nub'' [] = []
nub'' (h:t)
   | elem h t = nub'' t
   | otherwise = h : nub'' t

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

-- 29 -- Calcula a união de duas listas, sem repetir os elementos da segunda que ocorrem na primeira
union :: Eq a => [a] -> [a] -> [a]
union x [] = x
union [] y = y
union x (y:ys)
   | elem y x = union x ys
   | otherwise = union x' ys
   where x' = x ++ [y]

union' :: Eq a => [a] -> [a] -> [a]
union' x y = (++) x (filter (`notElem` x) y)

-- 30 -- Remove os elementos da primeira lista que não pertencem à segunda 
intersect :: Eq a => [a] -> [a] -> [a]
intersect x [] = []
intersect [] y = []
intersect (x:xs) y
   | elem x y = x : intersect xs y
   | otherwise = intersect xs y

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' x y = filter (`elem` y) x

-- 31 -- Insere ordenadamente um elemento na lista. 
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
   | h < x = h : insert x t
   | h >= x =  x : (h:t)
insert _ _ = []

-- 32 -- Junta as strings de uma lista com um espaço.
unwords' :: [String] -> String
unwords' = concatMap (flip (++) " ") -- o problema é aquele espaço no final, mas não há de ser nada de mais.

unwords'' :: [String] -> String
unwords'' [] = ""
unwords'' [s] = s ++ "."
unwords'' (h:t) = h ++ " " ++ unwords'' t

-- 33 -- Junta as strings de uma lisat com um "\newline".
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

unlines'' :: [String] -> String
unlines'' = concatMap (flip (++) "\n")

-- 34 -- Retorna o índice do maior elemento da lista 
pmaior :: Ord a => [a] -> Int
pmaior [] = 0
pmaior x = head (elemIndices'' (pmaior' x) x)

pmaior' :: Ord a => [a] -> a
pmaior' [x] = x
pmaior' (x:y:xs)
   | x < y = pmaior' (y:xs)
   | x >= y = pmaior' (x:xs)

-- 35 -- Filtra o primeiro b cujo a satisfaz a condição dada. (o enunciado da questão não bate certo com a definição)
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):t)
   | x == a = Just b
   | otherwise = lookup' x t

lookup'' :: Eq a => a -> [(a,b)] -> Maybe b
lookup'' a x = if null p then Nothing else Just $ (snd . head) p
   where p = filter (\(x',_) -> x' == a) x

-- 36(1) -- Calcula o maior prefixo crescente de uma lista 
prefixo :: Ord a => [a] -> [a]
prefixo (x:y:xs) = if x <= y then x : prefixo (y:xs) else [x]
prefixo [x] = [x]
prefixo _ = []

-- 36(2) -- Calcula o maior subconjunto crescente de uma lista 
preCrescente :: Ord a => [a] -> [[a]]
preCrescente [] = []
preCrescente [x] = [[x]]
preCrescente (x:y:t)
   | x <= y = (x : head r) : tail r
   | x > y = [x] : r
   where r = preCrescente (y:t)

preCrescente' :: Ord a => [[a]] -> [a]
preCrescente' [] = []
preCrescente' [x] = x
preCrescente' (x:y:t)
   | length x < length y = preCrescente' (y:t)
   | length x >= length y = preCrescente' (x:t)

preCrescente'' :: Ord a => [a] -> [a]
preCrescente'' [] = []
preCrescente'' x = preCrescente' (preCrescente x)

-- 37 -- Insertion Sort 
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

-- 38 -- True se a primeira string for menor lexicograficamente que a primeira, false caso contrário
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys)
   | x < y = True
   | x > y = False
   | x == y = menor xs ys

-- 39 -- Testa se o elemento pertence ao multiset 
type MSet a = [(a,Int)]

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x ((a,b):t)
   | x == a = True
   | otherwise = elemMSet x t

elemMSet' :: Eq a => a -> MSet a -> Bool
elemMSet' z = any (\(x,y) -> (==) x z) 

-- 40 -- Converte um multiset na lista dos seus elementos
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = replicate b a ++ converteMSet t

converteMSet' :: MSet a -> [a]
converteMSet' = concatMap (\(x,y) -> replicate y x) 

-- 41 -- Insere um elemento no multiset 
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t)
   | x == a = (a,b+1):t
   | otherwise = insereMSet x t

insereMSet' :: Eq a => a -> MSet a -> MSet a 
insereMSet' z ms = [if z == x then (x,y+1) else (x,y) | (x,y) <- ms]

-- 42 -- Remove um elemento ao multiconjunto
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t)
   | x == a = if b > 1 then (a,b-1):t else t
   | otherwise = removeMSet x t

removeMSet' :: Eq a => a -> MSet a -> MSet a 
removeMSet' z ms = filter (\(x,y) -> y /= 0) [if z == x then (x, y-1) else (x,y) | (x,y) <- ms]

-- 43 -- Constrói um multiset a partir de uma lista ordenada de a's.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet x = let r = auxMSet x 0
                 in (head x, r) : constroiMSet (drop r x)

auxMSet :: Ord a => [a] -> Int -> Int
auxMSet [] _ = 0
auxMSet [x] i = i + 1
auxMSet (x:y:ys) i
   | x == y = auxMSet (y:ys) (i+1)
   | otherwise = i + 1

constroiMSet' :: Ord a => [a] -> MSet a 
constroiMSet' [] = []
constroiMSet' x = (head p, r) : constroiMSet' (drop r x)
    where p = [y | y <- x, y == head x]
          r = length p 

-- 44 -- Divide uma lista de Eithers em duas listas
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left l):t) = (l : ls, rs)
   where (ls, rs) = partitionEithers' t
partitionEithers' ((Right r):t) = (ls, r : rs)
   where (ls, rs) = partitionEithers' t

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' [] = ([],[])
partitionEithers'' (x:xs) = case x of
                                  Left l -> (l : ls, rs)
                                  Right r -> (ls, r : rs)
   where (ls, rs) = partitionEithers'' xs


-- 45 -- Coleciona os elementos a de uma lista de maybes
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
                   Nothing -> catMaybes xs
                   Just p -> p : catMaybes xs

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just x : xs) = x : catMaybes' xs  

data Movimento = Norte | Sul | Este | Oeste
               deriving (Show, Eq)

caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (a,b) (c,d) =
   let x = c - a
       y = d - b
   in do
         let l1 = if x >= 0 then replicate x Este else replicate (-x) Oeste
         let l2 = if y >= 0 then replicate y Norte else replicate (-y) Sul
         return l1 ++ l2

hasloops :: [Movimento] -> Bool
hasloops [] = False
hasloops l
   | nmov Este l - nmov Oeste l == 0 && nmov Norte l - nmov Sul l == 0 = True
   | otherwise = hasloops $ init l

nmov :: Movimento -> [Movimento] -> Int
nmov _ [] = 0
nmov m (x:xs) = if x == m then 1 + nmov m xs else nmov m xs

type Ponto = (Float, Float)
data Rectangulo = Rect Ponto Ponto

contaRetangulo :: [Rectangulo] -> Int
contaRetangulo [] = 0
contaRetangulo (Rect (x1,y1) (x2, y2) : t)
   | abs (x2 - x1) == abs (y2 - y1) = 1 + contaRetangulo t
   | otherwise = contaRetangulo t

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (Rect (x1,y1) (x2, y2) : t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

data Equipamento = Bom | Razoavel | Avariado

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Avariado : t) = naoReparar t
naoReparar (_ : t) = 1 + naoReparar t
