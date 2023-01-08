-- Teste 18 -- 
import Data.List ( sort )
import System.Random ( randomRIO )

-- 1 -- 
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t)
   | x <= h = x : h : t 
   | x > h = h : insert x t 

_insert :: Ord a => a -> [a] -> [a]
_insert x l = let (a,b) = span (<= x) l in 
              a ++ [x] ++ b

-- 2 -- 
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr(\x acc -> case x of 
                               Just a  -> a : acc 
                               Nothing -> acc) []

catMaybes_ :: [Maybe a] -> [a] 
catMaybes_ [] = []
catMaybes_ ((Just x):t) = x : catMaybes_ t 
catMaybes_ (Nothing : t) = catMaybes_ t 

-- 3 -- 
data Exp a = Const a
           | Var   String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance (Show a) => Show (Exp a) where 
    show (Const x)    = show x 
    show (Var s)      = s 
    show (Mais e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

-- 4 --
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (x:xs) = sortOn f ys ++ [x] ++ sortOn f zs 
    where x' = f x 
          ys = [y | y <- xs, f y <= x']
          zs = [z | z <- xs, f z > x' ]

-- 5 ---
-- a -- 
amplitude :: [Int] -> Int 
amplitude [] = 0 
amplitude l = uncurry (-) . foldr(\x (_max, _min) -> (max x _max, min x _min)) (head l, head l) $ l

-- b --
geraAlt :: [Int] -> [([Int], [Int])]
geraAlt l = [ splitAt n l | n <- [1..length l - 1] ]

parte :: [Int] -> ([Int], [Int])
parte l = l' !! (snd . minimum . map (\((a,b), indx) -> (amplitude a + amplitude b, indx)) . flip zip [0..] $ l')
    where l' = geraAlt (sort l)

-- 6 --
data Imagem = Quadrado Int
             | Mover (Int,Int) Imagem
             | Juntar [Imagem] deriving Show 

ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),
                            Quadrado 4,
                            Mover (4,3) (Quadrado 3)])             
-- a --
conta :: Imagem -> Int 
conta (Quadrado _ ) = 1 
conta (Mover _ img) = conta img
conta (Juntar l )   = sum . map conta $ l 

-- b -- 
apaga :: Imagem -> IO Imagem 
apaga img = do let (a, b) = maxMinImg img 
               order <- randomRIO (a, b)
               return $ apagaOrd order img 

maxMinImg :: Imagem -> (Int, Int)
maxMinImg (Quadrado x)  = (x, x)
maxMinImg (Mover _ img) = maxMinImg img 
maxMinImg (Juntar [])   = (0, 0)
maxMinImg (Juntar lImg) = foldr(\(a, b) (accmin, accmax) -> 
                            (min a accmin, max b accmax)) (fst . head $ l, snd . head $ l) l 
    where l = map maxMinImg lImg 

apagaOrd :: Int -> Imagem -> Imagem 
apagaOrd x (Quadrado y)  = if (x == y) then Juntar [] else Quadrado y 
apagaOrd x (Mover p img) = Mover p (apagaOrd x img)
apagaOrd x (Juntar l)    = Juntar (map (apagaOrd x) l) 
