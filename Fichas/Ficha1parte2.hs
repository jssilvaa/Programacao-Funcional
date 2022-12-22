module Haskell.Ficha1parte2 where

import Data.Char ( ord , chr )
-- Algebraic Data Types -- 

-- Exercício 5 --

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop x = x == Vermelho

safe :: Semaforo -> Semaforo -> Bool
safe x y = not (x == Verde && y == Verde)

-- Exercício 6 -- 

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x*x + y*y)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
dist (Polar r a) (Cartesiano x y) = sqrt ((x - r * cos a)^2 + (y - r * sin a)^2)
dist (Cartesiano x y) (Polar r a) = sqrt ((x - r * cos a)^2 + (y - r * sin a)^2)
dist (Polar r1 a1) (Polar r2 a2) = sqrt ((r1 * cos a1 - r2 * cos a2)^2 + (r1 * sin a1 - r2  * sin a2)^2)

-- Exercício 7 -- 

data Figura = Circulo Ponto Double
             | Retangulo Ponto Ponto
             | Triangulo Ponto Ponto Ponto
             deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono f = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), Cartesiano (posx p2) (posy p1), p2]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

area :: Figura -> Double
area (Circulo _ r) = pi*r*r
area (Retangulo p1 p2) = abs ((posx p2 - posx p1) * (posy p2 - posy p1))
area (Triangulo p1 p2 p3) =
      let a = dist p1 p2
          b = dist p2 p3
          c = dist p3 p1
          s = (a+b+c) / 2
      in sqrt (s*(s-a)*(s-b)*(s-c))

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2*pi*r
perimetro (Retangulo p1 p2) = 2 * abs(posx p2 - posx p1) + 2 * abs(posy p2 - posy p1)
perimetro (Triangulo p1 p2 p3) =
            let a = dist p1 p2
                b = dist p2 p3
                c = dist p3 p1
            in a+b+c

isLower :: Char -> Bool
isLower c = ord 'a' <= ord c && ord c <= ord 'z'

isDigit :: Char -> Bool
isDigit c =  ord '0' <= ord c && ord c <=  ord '9'

isAlpha :: Char -> Bool
isAlpha c = (ord 'A' <= ord c && ord c <= ord 'Z') || (ord 'a' <= ord c && ord c <= ord 'z')

toUpper :: Char -> Char
toUpper c
  | ord 'a' <= ord c && ord c <= ord 'z' = chr (ord c + ord 'A' - ord 'a')
  | otherwise = c 

toLower :: Char -> Char
toLower c
  | ord 'A' <= ord c && ord c <= ord 'Z' = chr (ord c + ord 'a' - ord 'A')
  | otherwise = c

intToDigit :: Int -> Char
intToDigit i = chr (48 + i)

digitToInt :: Char -> Int
digitToInt c = ord c - 48

