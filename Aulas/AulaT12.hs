import System.Random

{- Conteúdo Lecionado 
1. Monads e ações; do, return e <-
2. Monad IO() 
3. Receber info. da consola e dar output a outra info.
4. Classe Random 

Funções utilizadas:
 
return :: a -> IO a 
getChar :: IO Char 
getLine :: IO String 
putChr :: Char -> IO ()
putStr :: String -> IO ()
random :: Random a => IO a 
randomRIO :: Random a => (a, a) -> IO a

Cuidados: indentação num bloco de ações "do"

Termos conceptuais
- Ação
- IO 
-}

-- Funções pré-definidas -- 
getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then return ""
              else do xs <- getLine'
                      return (x:xs)
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (h:t) = do putChar h 
                   putStr' t 

putStrLn' :: String -> IO ()
putStrLn' l = do putStr l 
                 putChar '\n'  

---- Aplicações 
-- Input/Output elementar
pergResp :: String -> IO String 
pergResp p = do putStrLn p
                getLine  

pergRespResp :: String -> IO ()
pergRespResp p = do putStrLn p 
                    r <- getLine
                    putStrLn r

-- Lançar um par de dados 
lanca2Dados :: IO (Int, Int)
lanca2Dados = do x <- randomRIO (1,6)
                 y <- randomRIO (1,6)
                 return (x,y)

-- Lançar N dados             
lancaNDados :: Int -> IO [Int]
lancaNDados 0 =  return []
lancaNDados n = do x <- randomRIO (1,6)
                   xs <- lancaNDados (n-1)
                   return (x:xs)

-- baralha :: [a] -> IO [a]
baralha [] = return []
baralha (h:t) = do tb <- baralha t
                   p <- randomRIO (0, length t - 1) 
                   let (a,b) = splitAt p tb 
                   return (a ++ [h] ++ b)
