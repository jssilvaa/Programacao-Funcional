-- Teste 17 -- 
import Data.Maybe (mapMaybe)

-- 1 -- 
type MSet a = [(a, Int)] 
mset = [('b', 3),('a', 2),('c', 1),('d', 1)] :: MSet Char 

-- a --
cardMSet :: MSet a -> Int 
cardMSet = foldr (\(_, m) acc -> m + acc) 0 

-- b --
moda :: MSet a -> [a] 
moda ms = (map fst) . takeWhile (\(a, b) -> b == m) $ ms 
    where m = snd . head $ ms

-- c --
converteMSet :: MSet a -> [a]
converteMSet = foldr(\(a, b) l -> (replicate b a) ++ l) []

-- d --
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a 
addNcopies [] x num = [(x, num)] 
addNcopies ((a, b) : t) x num
   | x == a = (a, b + num) : t 
   | otherwise = placeIn (a, b) (addNcopies t x num)
   where 
         placeIn :: (a, Int) -> MSet a -> MSet a 
         placeIn (a,b) [] = [(a,b)] 
         placeIn (a,b) ((x,y):t)
            | b >= y = (a, b) : (x, y) : t
            | otherwise = (x,y) : placeIn (a,b) t 

-- 2 --
data SReais = AA Double Double | FF Double Double
            | AF Double Double | FA Double Double
            | Uniao SReais SReais

intervalo = Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0)

-- a -- 
instance Show SReais where 
    show (AA x y)      = "]" ++ show x ++ "," ++ show y ++ "["
    show (AF x y)      = "]" ++ show x ++ "," ++ show y ++ "]"
    show (FA x y)      = "[" ++ show x ++ "," ++ show y ++ "["
    show (FF x y)      = "[" ++ show x ++ "," ++ show y ++ "]"
    show (Uniao s1 s2) = "(" ++ show s1 ++ " U " ++ show s2 ++ ")"

-- b -- 
pertence :: Double -> SReais -> Bool 
pertence x (AA a b)      = x > a && x < b 
pertence x (AF a b)      = x > a && x <= b 
pertence x (FA a b)      = x >= a && x < b 
pertence x (FF a b)      = x >= a && x <= b 
pertence x (Uniao s1 s2) = pertence x s1 || pertence x s2 

-- c -- 
tira :: Double -> SReais -> SReais 
tira x (AA a b)
    | x > a && x < b = Uniao (AA a x) (AA x b)
    | otherwise      = AA a b 
tira x (AF a b)
    | x > a && x < b = Uniao (AA a x) (AF x b)
    | x == b         = AA a b 
    | otherwise      = AF a b 
tira x (FA a b)
    | x > a && x < b = Uniao (FA a x) (AA x b)
    | x == a         = AA a b 
    | otherwise      = FA a b 
tira x (FF a b)
    | x > a && x < b = Uniao (FA a x) (AF x b)
    | x == a         = AF a b 
    | x == b         = FA a b 
    | otherwise      = FF a b 

-- 3 -- 
data RTree a = R a [RTree a]

sample = R 1 [R 2 [], R 3 [R 4 [R 5 []]]]
path = [2,1,1] :: [Int]

-- a --
percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R x _) = Just [x]
percorre _ (R x []) = Just [x]
percorre (h:t) (R x l) 
    | h > 0 && h <= length l = let p = h - 1 in 
                              fmap (x :) (percorre t (l !! p))
    | otherwise = Nothing 

-- b --
procura :: Eq a => a -> RTree a -> Maybe [Integer]
procura x (R y []) = if (x == y) then Just [] else Nothing 
procura x (R y l) 
   | x == y    = Just []
   | otherwise =  Just $ head . mapMaybe id . map snd . map (\(indx, maybe_path) -> 
    (indx, (indx :) <$> maybe_path)) . map (\(indx, rtree) -> (indx, procura x rtree)) $ zip [1..] l
