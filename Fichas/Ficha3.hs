-- Ficha 3 --
import Haskell.Ficha1parte2

-- 2 --
type Poligonal = [Ponto]

linha :: Poligonal -> Double
linha [] = 0
linha [x] = 0
linha (x:y:xs) = dist x y + linha (y:xs)

fechada :: Poligonal -> Bool
fechada [] = False
fechada [x] = False
fechada l = head l == last l

triangula :: Poligonal -> [Figura]
triangula [] = []
triangula (x:y:z:t) = Triangulo x y z : triangula (x:z:t)
triangula p = []

area' :: Poligonal -> Double
area' p = sum $ map area $ triangula p

psoma :: Ponto -> Ponto -> Ponto
psoma u v = Cartesiano (posx u + posx v) (posy u + posy v)

mover :: Poligonal -> Ponto -> Poligonal
mover [] _ = []
mover p q = let s = Cartesiano (posx q - posx (head p )) (posy q - posy (head p))
            in mover' p s

mover' :: Poligonal -> Ponto -> Poligonal
mover' [] _ = []
mover' (h:t) s = psoma h s : mover' t s

mover'' :: Poligonal -> Ponto -> Poligonal 
mover'' [] _ = []
mover'' p q = map (psoma s) p 
   where s = Cartesiano (posx q - posx (head p )) (posy q - posy (head p))

-- Exemplos: Translação do triângulo de volta à origem 
exPoligono :: [Ponto]
exPoligono = [Cartesiano 2 2, Cartesiano 6 2, Cartesiano 4 6]
origem :: Ponto
origem = Cartesiano 0 0
-- 

pmultiplica :: Double -> Ponto -> Ponto 
pmultiplica k x = Cartesiano (k * posx x) (k * posy x)

displacement :: Ponto -> Ponto -> Ponto 
displacement x y = Cartesiano (posx x - posx y) (posy x - posy y)

zoom :: Double -> Poligonal -> Poligonal 
zoom _ [] = []
zoom k [p] = [p]
zoom k (p:q:t) = psoma p (pmultiplica k (displacement q p)) : zoom k (p:t)

exQuadrado :: [Ponto]
exQuadrado = [Cartesiano 0 0, Cartesiano 2 0, Cartesiano 2 2, Cartesiano 0 2]


-- 3 -- 
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
            deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

agendaEx = [("Sofia", [Email "sofia@email.com", Tlm 912345678]), ("Joao", [Email "joao@email.com", Trab 253987654])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nm em [] = [(nm, [Email em])]
acrescEmail nm em ((nm_a, ctc):t)
    | nm == nm_a = (nm, Email em : ctc) : t
    | otherwise = (nm_a, ctc) : acrescEmail nm em t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((nome_a, contactos):t)
   | nome /= nome_a = verEmails nome t
   | otherwise = Just $ filtraEmails contactos

filtraEmails :: [Contacto] -> [String]
filtraEmails [] = []
filtraEmails (Email e : t) = e : filtraEmails t
filtraEmails (_:t) = filtraEmails t

consTelfs :: [Contacto] -> [Integer]
consTelfs [] = []
consTelfs (x:xs) = case x of
                        Email e -> consTelfs xs
                        Casa tlf -> tlf : consTelfs xs
                        Trab tlf -> tlf : consTelfs xs
                        Tlm tlm -> tlm : consTelfs xs

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((nome_a, contactos):t)
   | nome /= nome_a = casa nome t
   | otherwise = let p = filtraCasa contactos
                 in if p == 0 then Nothing else Just p

filtraCasa :: [Contacto] -> Integer
filtraCasa [] = 0
filtraCasa (Casa tlf : t) = tlf
filtraCasa (h:t) = filtraCasa t

-- 4 -- 

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
        deriving Show

type TabDN = [(Nome, Data)]

today :: Data
today = D 22 10 2022
sample :: Data
sample = D 31 07 2013
sample1 :: Data
sample1 = D 20 10 2010

tabEx :: [(Nome, Data)]
tabEx = [("Jose", D 08 05 2004), ("Mariana", D 20 08 2010), ("Mae",D 19 10 1978), ("Pai", D 08 12 1977)]

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura x ((p, q):t)
   | x == p = Just q
   | otherwise = procura x t

idade :: Data -> Nome -> TabDN -> Maybe Int
idade d1 n tab
  = do d2 <- procura n tab
       return $ datacmp d1 d2

datacmp :: Data -> Data -> Int
datacmp (D d1 m1 a1) (D d2 m2 a2)
   | m1 > m2 = a1 - a2
   | m1 == m2 = if d1 >= d2 then a1 - a2 else a1 - a2 - 1
   | otherwise = a1 - a2 - 1

dataeq :: Data -> Data -> Bool
dataeq (D d1 m1 a1) (D d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
   | a1 < a2 = True
   | a1 == a2 && m1 < m2 = True
   | a1 == a2 && m1 == m2 && d1 < d2 = True
   | otherwise = False

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((nome, d):t) = ordena ys ++ [(nome, d)] ++ ordena zs
   where (ys, zs) = partEficiente d t

partEficiente :: Data -> TabDN -> (TabDN, TabDN)
partEficiente _ [] = ([],[])
partEficiente d' ((nome, d):t) = if anterior d d' then ((nome, d) : ls, rs) else (ls, (nome,d):rs)
   where (ls, rs) = partEficiente d t

porIdade :: Data -> TabDN -> [(Nome, Int)]
porIdade _ [] = []
porIdade d ((nome, dn):t) = (nome, calculaIdade d dn) : porIdade d t

calculaIdade :: Data -> Data -> Int
calculaIdade (D d m a) (D d' m' a')
  | m > m' = a - a'
  | d >= d' = a - a'
  | otherwise = a - a' - 1

-- 5 -- 

data Movimento = Credito Float | Debito Float
   deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
   deriving Show

extrato :: Extracto
extrato = Ext 560 [(D 23 10 2022, "Compra na Amazon", Debito 400), (D 25 10 2022, "Bolsa", Credito 2000)]

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) v = []
extValor (Ext a ((x,y,Credito z):t)) v
   | z < v = extValor (Ext a t) v
   | otherwise = Credito z : extValor (Ext a t) v
extValor (Ext a ((x,y,Debito z):t)) v
   | z < v = extValor (Ext a t) v
   | otherwise = Debito z : extValor (Ext a t) v

filtro :: Extracto -> [String] -> [(Data, Movimento)]
filtro _ [] = []
filtro (Ext _ []) _ = []
filtro (Ext a ((x,y,z):t)) s
   | y `elem` s = (x,z) : filtro (Ext a t) s
   | otherwise = filtro (Ext a t) s

creDeb :: Extracto -> (Float, Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext a ((x,y, Credito z):t)) = (z+ls, rs)
   where (ls,rs) = creDeb (Ext a t)
creDeb (Ext a ((x,y, Debito z):t)) = (ls, z+rs)
   where (ls,rs) = creDeb (Ext a t)

saldo :: Extracto -> Float
saldo (Ext saldo xs) = let (cre, deb) = creDeb (Ext saldo xs)
                       in saldo + cre - deb

