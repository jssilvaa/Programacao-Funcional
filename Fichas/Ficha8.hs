-- Ficha 8 :: Classe de Tipos -- 
import Data.List ( sortBy )

-- 1 -- 
data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (mod x y)

normaliza :: Frac -> Frac
normaliza (F a b) = F (div a m) (div b m)
    where m = mdc a b

instance Eq Frac where
    (==) :: Frac -> Frac -> Bool
    (==) (F x y) (F z w) = x*w == y*z

instance Ord Frac where
    (<=) :: Frac -> Frac -> Bool
    (<=) (F x y) (F z w) = x*w <= y*z

instance Show Frac where
    show :: Frac -> String
    show (F x y) = show x ++ "/" ++ show y

instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (+) (F x y) (F z w) = normaliza (F a b)
       where a = x*w + z*y
             b = y*w
    (*) :: Frac -> Frac -> Frac
    (*) (F x y) (F z w) = normaliza (F a b)
       where (a,b) = (x*z,y*w)
    abs :: Frac -> Frac
    abs (F x y) = if z == 1 then F x y else F (negate x) y
       where z = signum x * signum y
    signum :: Frac -> Frac
    signum (F x y) = let a = signum x * signum y
                     in F a 1
    fromInteger :: Integer -> Frac 
    fromInteger x = F x 1 
    negate :: Frac -> Frac 
    negate (F x y) = F (negate x) y

filtra :: Frac -> [Frac] -> [Frac] 
filtra _ [] = []
filtra x l = filter (\y -> y >= 2 * x) l

-- 2 --

data Exp a = Const a | Simetrico (Exp a) | Mais (Exp a) (Exp a) | Menos (Exp a) (Exp a) | Mult (Exp a) (Exp a)

calcula :: (Num a) => Exp a -> a 
calcula (Const x) = x 
calcula (Simetrico x) = negate (calcula x)
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

instance (Show a) => Show (Exp a) where
    show :: Exp a -> String 
    show (Const a) = show a 
    show (Simetrico e) = "- " ++ show e 
    show (Mais e1 e2) = show e1 ++ " + " ++ show e2 
    show (Menos e1 e2) = show e1 ++ " - " ++ show e2 
    show (Mult e1 e2) = show e1 ++ " * " ++ show e2 

instance (Eq a) => Eq (Exp a) where 
    (==) :: Exp a -> Exp a -> Bool 
    (==) (Const a) (Const b) = a == b 
    (==) (Simetrico a) (Simetrico b) = a == b 
    (==) (Mais e1 e2) (Mais e3 e4) = e1 == e3 && e2 == e4 
    (==) (Menos e1 e2) (Menos e3 e4) = e1 == e3 && e2 == e4 
    (==) (Mult e1 e2) (Mult e3 e4) = e1 == e3 && e2 == e4 
    (==) _ _ = False 

instance (Ord a, Num a) => Num (Exp a) where 
    (+) :: Exp a -> Exp a -> Exp a 
    (+) = Mais 
    (*) :: Exp a -> Exp a -> Exp a 
    (*) = Mult 
    (-) :: Exp a -> Exp a -> Exp a 
    (-) = Menos
    -- negate :: Exp a -> Exp a 
    -- negate = Simetrico 
    abs :: (Ord a) => Exp a -> Exp a 
    abs e | calcula e < 0 = Simetrico e 
          | otherwise = e 
    signum :: Exp a -> Exp a 
    signum e | calcula e > 0 = 1 
             | calcula e == 0 = 0
             | otherwise = -1
    fromInteger :: Integer -> Exp a 
    fromInteger a = Const (fromInteger a)

-- 3 -- 
data Movimento = Credito Float | Debito Float 
data Data = D Int Int Int 
data Extracto = Ext Float [(Data, String, Movimento)]

-- 3 a --
instance Eq Data where 
    (==) (D d1 m1 a1) (D d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2 

instance Ord Data where 
    compare (D d1 m1 a1) (D d2 m2 a2) 
       | a1 > a2 = GT 
       | a1 < a2 = LT 
       | m1 > m2 = GT 
       | m1 < m2 = LT 
       | d1 > d2 = GT 
       | d1 < d2 = LT 
       | otherwise = EQ 

-- 3 b -- 
instance Show Data where 
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d

-- 3 c -- 
ordena :: Extracto -> Extracto
ordena (Ext a l) = Ext a (sortBy sortData l)
    where sortData (a,_,_) (b,_,_) | a < b = GT
                                   | a == b = EQ  
                                   | a > b = LT
-- 3 d -- 

-- sample extract -- 
sample = Ext 300 [(D 5 4 2010, "DEPOSITO", Credito 2000), (D 10 8 2010, "COMPRA", Debito 37.5), (D 1 9 2010, "LEV", Debito 60),
                  (D 7 1 2011, "JUROS", Credito 100), (D 22 1 2011, "ANUIDADE", Debito 8)]

calcSaldo :: Extracto -> Float 
calcSaldo (Ext vi l) = vi + (sum . map (\(_,_,mov) -> (case mov of Credito x -> x
                                                                   Debito  y -> negate y))) l 

instance Show Extracto where
    show (Ext saldo lista) = unlines (["Saldo anterior: " ++ show saldo,
                                      "---------------------------------------",
                                      "Data       Descricao   Cred   Debito",
                                      "---------------------------------------"
                                      ] ++ movs l' ++ [
                                      "---------------------------------------",
                                      "Saldo atual: " ++ show sAtual])
        where sAtual = calcSaldo (Ext saldo lista )
              (Ext _ l') = ordena (Ext saldo lista)
              movs = map (\(date, descr, mov) -> 
                show date ++ (replicate (11 - length (show date)) ' ') ++ descr ++ (replicate (12 - length descr) ' ') 
                ++ ((case mov of Credito x -> show x 
                                 Debito  y -> (replicate 7 ' ') ++ show y)))
            
