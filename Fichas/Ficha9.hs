-- Ficha 9 :: IO -- 
import System.Random ( randomRIO )
import Data.List ( splitAt, nub )
import Data.Bifunctor ( bimap )

-- 1 a --
bingoAux :: [Int] -> IO ()
bingoAux [] = return ()
bingoAux l = do getChar 
                j <- randomRIO (0, length l - 1)
                putStr $ show (l !! j)
                let (left,right) = splitAt j l 
                bingoAux (left ++ (tail right))

bingo :: IO ()
bingo = bingoAux [1..90]

-- 1 b -- 
geraSeq :: IO String 
geraSeq = do first  <- randomRIO ('0', '9')
             second <- randomRIO ('0', '9')
             third  <- randomRIO ('0', '9')
             fourth <- randomRIO ('0', '9')
             return (first:second:third:fourth:[])


mastermindAux :: String -> IO ()
mastermindAux seq = do str <- getLine 
                       let p = (length . filter (\x -> x) $ (map (`elem` seq) str)) 
                       let q = (length . filter (\x -> x) $ zipWith (\x y -> x == y) seq str)
                       if q == 4 then putStrLn ("Parabéns! Acertaste na sequência: " ++ (show seq))
                                 else do putStrLn ("Número de dígitos corretos na posição correta: "   ++ (show q))
                                         putStrLn ("Número de dígitos corretos na posição incorreta: " ++ (show (p - q)))
                                         mastermindAux seq 

mastermind :: IO ()
mastermind = do seq <- geraSeq 
                mastermindAux seq 

-- 2 -- 
data Aposta = Ap [Int] (Int,Int)

-- 2 a --
valida :: Aposta -> Bool 
valida (Ap l (x,y)) = isValidS (x,y) && isValidL l 
    where isValidS :: (Int,Int) -> Bool 
          isValidS (x,y) = x /= y && 0 < x && x < 10 && 0 < y && y < 10 
          isValidL :: [Int] -> Bool 
          isValidL l = ((== 5) . length . filter (\x -> x) . map (\x -> 0 < x && x <= 50)) l && (length . nub) l == 5 

-- 2 b --
comuns :: Aposta -> Aposta -> (Int, Int)
comuns (Ap l (x,y)) (Ap r (z,w)) = (comunsL l r, comunsS (x,y) (z,w))
    where comunsS :: (Int,Int) -> (Int,Int) -> Int
          comunsS (a,b) (c,d) = (length . filter (\x -> x)) [a == c || b == c, a == d || b == d]
          comunsL :: [Int] -> [Int] -> Int 
          comunsL l1 = length . filter (\x -> x) . map (`elem` l1) 


-- 2 c -- 
-- i -- 
instance Eq Aposta where 
    (==) x y = comuns x y == (5,2)

instance Show Aposta where 
    show (Ap l (x,y)) = "Números " ++ show l ++ "\nEstrelas: " ++ show x ++ " " ++ show y 

-- ii -- 
premio :: Aposta -> Aposta -> Maybe Int 
premio x y = lookup (comuns x y) premios 
    where premios = zip [(5,2),(5,1),(5,0),(4,2),(4,1),(4,0),(3,2),(2,2),(3,1),(3,0),(1,2),(2,1),(2,0)] [1..]
           
-- 2 d -- 
-- i --
leAposta :: IO Aposta 
leAposta = do putStrLn "Introduza os números, um em cada linha: "
              putStr "> "
              n1 <- getLine 
              n2 <- getLine 
              n3 <- getLine 
              n4 <- getLine 
              n5 <- getLine 
              putStrLn "Introduza as estrelas, uma em cada linha: "
              putStr "> "
              e1 <- getLine
              e2 <- getLine 
              let l = map read [n1,n2,n3,n4,n5]
              let e = bimap read read (e1,e2)
              let aposta = Ap l e 
              if valida aposta then return aposta else putStrLn "Aposta inválida." >> leAposta 

-- ii --
joga :: Aposta -> IO ()
joga key = leAposta >>= (\x -> print $ premio x key)
        
-- 2 e -- 
geraChave :: IO Aposta 
geraChave = do n1 <- randomRIO (1,50)
               n2 <- randomRIO (1,50)
               n3 <- randomRIO (1,50)
               n4 <- randomRIO (1,50)
               n5 <- randomRIO (1,50)
               let l = [n1,n2,n3,n4,n5]
               e1 <- randomRIO (1,9)
               e2 <- randomRIO (1,9)
               let e = (,) e1 e2 
               let ap = Ap l e 
               if valida ap then return ap else geraChave 

-- 2 f -- 
main :: IO ()
main = do key <- geraChave 
          ciclo key 

ciclo :: Aposta -> IO ()
ciclo key = do option <- menu 
               case option of "0" -> putStrLn "Obrigado por jogar!"
                              "1" -> joga key
                              "2" -> main 

menu :: IO String 
menu = do putStrLn menutxt
          putStr "> Option: "
          c <- getLine 
          return c 
  where menutxt = unlines ["", 
                           "Apostar ............ 1", 
                           "Gerar nova chave  .. 2", 
                           "", 
                           "Sair ............... 0"]
