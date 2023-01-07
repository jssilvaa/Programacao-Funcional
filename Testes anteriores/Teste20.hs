-- Teste 20 -- 
import Data.List ( intercalate, groupBy )

-- 1 -- 
-- a --
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (h:t) l 
    | h `elem` l = h : intersect t l
    | otherwise = intersect t l 

-- b --
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

-- 2 --
type ConjInt = [Intervalo] 
type Intervalo = (Int, Int)

-- a --
elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):t) = [a..b] ++ elems t 

-- b -- 
geraConj :: [Int] -> ConjInt 
geraConj [] = []
geraConj [x] = (x,x) : []
geraConj (h:t) 
    | h == x-1 = (h,y) : zs 
    | otherwise = (h,h) : (x,y) : zs 
   where (x,y):zs = geraConj t 

-- 3 -- 
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
type Nome = String
type Agenda = [(Nome, [Contacto])] 

-- a -- 
acrescEmail :: Nome -> String -> Agenda -> Agenda 
acrescEmail nome email [] = [(nome, [Email email])]
acrescEmail nome email ((nome_ag, l) : t)
    | nome == nome_ag = (nome, (Email email) : l) : t 
    | otherwise = (nome_ag, l) : acrescEmail nome email t 

-- b -- 
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing 
verEmails nome ((nome_ag, l):t)
    | nome == nome_ag = Just $ filtraEmails l 
    | nome /= nome_ag = verEmails nome t 

filtraEmails :: [Contacto] -> [String]
filtraEmails [] = []
filtraEmails ((Email e):t) = e : filtraEmails t 
filtraEmails (_:t) = filtraEmails t 

-- c --
consulta :: [Contacto] -> ([Integer], [String])
consulta = foldr (\x (tlfs, emails) -> 
        case x of Casa num -> (num : tlfs, emails)
                  Trab num -> (num : tlfs, emails)
                  Tlm num  -> (num : tlfs, emails)
                  Email e  -> (tlfs, e : emails)
                  ) ([],[])

-- d --
consultaIO :: Agenda -> IO()
consultaIO ag = do putStr "> "
                   nome <- getLine 
                   let (tlfs, emails) = consultaNome nome ag 
                   let tlfs_str = intercalate ", " $ map show tlfs 
                   let emails_str = intercalate ", " emails 
                   putStrLn ("Numeros de telefone/telemovel: " ++ tlfs_str)
                   putStrLn ("Emails: " ++ emails_str)

consultaNome :: Nome -> Agenda -> ([Integer], [String])
consultaNome nome [] = ([],[])
consultaNome nome ((nome_ag, l):t) 
    | nome == nome_ag = consulta l 
    | otherwise = consultaNome nome t 

-- 4 -- 
data RTree a = R a [RTree a] deriving Show 

-- a -- 
paths :: RTree a -> [[a]]
paths (R e []) = [[e]]
paths (R e l) = map ((:) e) $ concatMap paths l

-- b -- 
unpaths :: Eq a => [[a]] -> RTree a 
unpaths [[x]] = R x []
unpaths l = let r = head . head $ l 
            in R r (map unpaths $ 
                    groupBy (\a b -> head a == head b)
                    $ map tail l)

