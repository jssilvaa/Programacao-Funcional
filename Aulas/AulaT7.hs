-- Acumuladores -- Aula Prática -- 

-- Definição da função soma com acumulador 

somaAcc :: Num a => a -> [a] -> a
somaAcc acc [] = acc
somaAcc acc (h:t) = somaAcc (acc + h) t

soma :: Num a => [a] -> a
soma = somaAcc 0

-- Definição da função produto com acumulador 

prodAcc :: Num a => a -> [a] -> a
prodAcc acc [] = acc
prodAcc acc (h:t) = prodAcc (acc * h) t

product :: Num a => [a] -> a
product = prodAcc 1

-- Funções de ordem superior -- Aula teórica -- 

{- The professor decided, on his approach to our learning of functions in haskell, to 
hoodwink us. The actual definition of a function is f :: (args) -> (result). The arrow (->) operation 
is right associative, and therefore a function really only has one argument per call. That is, a function with multiple 
arguments will receive one argument, and pass the other arguments to a more appropriate function. 

For instance, let's have a look to the (+) function. One possible type signature might be: 

(+) :: Num a => a -> a -> a 

Which, in truth, is read as: 

(+) :: Num a => a -> (a -> a)

That is, the function (+) receives one numeric type as its argument and YIELDS another function (+ k) :: a -> a 
that receives a numeric a and outputs a numeric a (k is the value passed to the original function).  

This concept motivates the notion for higher-order functions: functions that take functions as arguments, 
which can be properties (conditions), operations, etc. 

Consideremos os seguintes exemplos no sentido de motivar o abuso destas funções.
-}

succ' :: Num a => a -> a
succ' x = 1 + x

{- A função succ' (succ na prelude) recebe um numérico e retorna a sua soma com um. Esta definição é perfeitamente plausível.
No entanto, olhando para a definição da função (+), isto é o mesmo que escrever: -}

succ'' :: Num a => a -> a
succ'' x = (+) 1 x

-- Como (->) é right-associative, então podemos escrever isto da seguinte maneira -- 

succ''' :: Num a => a -> a
succ''' x = ((+) 1) x

-- E então vemos que a transformação à direita tem exatamente o mesmo resultado para todo o valor em A, se succ :: A -> B
-- Que a primeira função succ' definida. Logo, tem-se succ''' = (+) 1 

succ'''' :: Num a => a -> a
succ'''' = (+) 1

-- E se o objetivo for aplicar a função sucessor, não a um número, mas a um vetor de números? 

succS :: Num a => [a] -> [a]
succS [] = []
succS (h:t) = succ'''' h : succS t

-- Notamos que há um padrão semelhante para outras operações a serem efetuadas em listas. 

dobro :: Num a => a -> a
dobro = (* 2)

dobroS :: Num a => [a] -> [a]
dobroS [] = []
dobroS (h:t) = dobro h : dobroS t

{-
O padrão da forma 
g :: (?) -> [a] -> [b]
g _ [] = []
g f (h:t) = f h : g t 
onde f :: a -> b
repete-se nestes e em muitos outros casos, quando queremos percorrer uma sequência 
e efetuar uma operação idêntica para todos os elementos. 
Para isso, definimos a função map:
 -}

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Pelo que redefinimos as funções succS e dobroS como -- 

succS' :: Num a => [a] -> [a]
succS' = map (+1)

dobroS' :: Num a => [a] -> [a]
dobroS' = map (*2)

-- Agora, tratemos do caso em que desejamos filtrar um subconjunto de elementos de uma lista que satisfaça uma propriedade 
-- Isto é mais um padrão dos vários que demonstram o quão compactos podemos ser em haskell

positivos :: (Num a, Ord a) => [a] -> [a]
positivos [] = []
positivos (h:t) | h > 0 = h : positivos t
                | otherwise = positivos t

impares :: [Int] -> [Int]
impares [] = []
impares (h:t) | mod h 2 /= 0 = h : impares t
              | otherwise = impares t

digitos :: String -> String
digitos [] = []
digitos (h:t) | h >= '0' && h <= '9' = h : digitos t
              | otherwise = digitos t

{- Este padrão de uma função g : A - B onde B é um subconjunto de A que satisfaz uma propriedade P 
pode ser posto da maneira 

g :: (a -> Bool) -> [a] -> [a]
g _ [] = []
g p (h:t) | p h = h : g p t 
          | otherwise = g p t

Esta função está definida em haskell e designa-se filter. 
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (h:t) | f h = h : filter' f t
                | otherwise = filter' f t

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []

-- Pelo que reescrevemos as funções como:

positivos' :: (Num a, Ord a) => [a] -> [a]
positivos' = filter' (>0)

impares' :: [Int] -> [Int]
impares' = filter' (\x -> mod x 2 /= 0)

digitos' :: [Char] -> [Char] 
digitos' = filter (\x -> x >= '0' && x <= '9')

-- Próximas funções a escrever: zip e zipwith, mais aplicações.

{- In this very same lesson we learned how to mess with two lists at the same time. 
Considering two non-empty lists l1 and l2, it might be that we desire to add each element at the same 
time, as for instance when defining sums of vectors, of matrices. We might want to zip them together, to form 
yet a new list, containg pairs of elements of l1 and l2 which occur at the same positions. -}

zip' :: [a] -> [b] -> [(a,b)] 
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 
zip' _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 
zipWith' _ _ _ = []

-- For instance, we might define sums of vectors in the following way: 
type Vector = [Double]

vectorSum :: Vector -> Vector -> Vector 
vectorSum = zipWith' (+) 

-- Even more general, we might define sums of matrices as follows: 
type Matrix = [Vector]

matrixSum :: Matrix -> Matrix -> Matrix 
matrixSum = zipWith' (zipWith' (+)) -- alternativamente, podemos fazer zipWith' . zipWith' $ (+)
