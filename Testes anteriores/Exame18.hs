-- Exame 18 -- 
import Data.Char ( toUpper )
import Data.Maybe ( mapMaybe )
import Data.List ( permutations, sortOn )


-- 1 --
-- (!!) :: [a] -> Int -> a 
-- (!!) l n = fst . head . filter (\(a,b) -> b == n) $ flip zip [0..] l

-- 2 -- 
data Movimento = Norte | Sul | Este | Oeste 

posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
posicao = foldr(\mov (x,y) -> case mov of 
                                        Norte -> (x, y + 1)
                                        Sul   -> (x, y - 1)
                                        Este  -> (x + 1, y)
                                        Oeste -> (x - 1, y))

-- 3 --
any' :: (a -> Bool) -> [a] -> Bool 
any' _ [] = False 
any' f (h:t) = f h || any' f t 


_any :: (a -> Bool) -> [a] -> Bool 
_any f = or . map f  

-- 4 -- Nota que a classe num não tem herda automaticamente Eq a, devemos fazer questão de o especificar. 
type Mat a = [[a]]
mat = [[1,2,3], [0,4,5], [0,0,6]] 

triSup :: (Eq a, Num a) => Mat a -> Bool 
triSup m = all (all (== 0)) [take n (m !! n) | n <- [0..length m - 1]]

_triSup :: (Eq a, Num a) => Mat a -> Bool 
_triSup = all (all (== 0) . uncurry take) . zip [0..] 

-- 5 --
movimenta :: IO (Int, Int)
movimenta = do c <- getChar 
               case (toUpper c) of 
                    'N' -> movimentaAux (0, 1) 
                    'S' -> movimentaAux (0, -1)
                    'E' -> movimentaAux (1, 0)
                    'O' -> movimentaAux (-1, 0)
                    _   -> return (0,0)


movimentaAux :: (Int, Int) -> IO (Int, Int)
movimentaAux (x,y) = do c <- getChar
                        case (toUpper c )of 
                            'N' -> movimentaAux (x, y + 1) 
                            'S' -> movimentaAux (x, y - 1)
                            'E' -> movimentaAux (x + 1, y)
                            'O' -> movimentaAux (x - 1, y)
                            _ -> return (x,y)
-- 6 --
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem] deriving Show 

ex :: Imagem
ex = Mover (5,5)
      (Juntar [Mover (0,1) (Quadrado 5),
              Quadrado 4,
              Mover (4,3) (Quadrado 2)])

exVazio = Mover (5,5)
          (Juntar [Mover (0,1) (Juntar []),
                  Juntar [],
                  Mover (4,3) (Juntar [])])

exx = Juntar [Mover (5,5) (Quadrado 4), Mover (5,6) (Quadrado 5), Mover (9,8) (Quadrado 2)]

-- a -- 
vazia :: Imagem -> Bool 
vazia (Quadrado _)  = False 
vazia (Mover _ img) = vazia img 
vazia (Juntar [])   = True 
vazia (Juntar l)    = and . map vazia $ l 

-- b -- 
maior :: Imagem -> Maybe Int  
maior (Quadrado x)  = Just x 
maior (Mover _ img) = maior img 
maior (Juntar [])   = Nothing 
maior (Juntar l)    = case (mapMaybe maior l) of 
                                [] -> Nothing 
                                l  -> Just $ maximum l 

-- c -- 
instance Eq Imagem where 
    (==) (Mover (x,y) (Quadrado l)) (Mover (x', y') (Quadrado l')) = x == x' && y == y' && l == l' 
    (==) img1 img2 = let (x:xs) = simplificaPos img1 
                         (y:ys) = simplificaPos img2 in
                     x == y && xs == ys

toPos :: Imagem -> [(Imagem, (Int, Int))]
toPos (Quadrado a)      = [(Quadrado a, (0, 0))] 
toPos (Mover (x,y) img) = map (\(img, (x', y')) -> (img, (x+x', y+y'))) $ toPos img 
toPos (Juntar l)        = concatMap toPos l  

toMover :: [(Imagem, (Int, Int))] -> [Imagem]
toMover [] = []
toMover ((img, (x,y)):t) = (Mover (x,y) img) : toMover t 

sortImg :: [Imagem] -> [Imagem]
sortImg = sortOn (\(Mover _ (Quadrado x)) -> x) 

simplificaPos :: Imagem -> [Imagem] 
simplificaPos = sortImg . toMover . toPos 
