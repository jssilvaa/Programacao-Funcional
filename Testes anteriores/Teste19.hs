-- Teste 18/19 -- 
import Data.List ( intercalate, transpose, nub )
import System.Random ( randomRIO )

-- 1 -- 
-- a --
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x (h:t) 
   | x == h    = 0 : map (+1) (elemIndices x t) 
   | otherwise = map (+1) (elemIndices x t) 

-- b -- 
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf [] _ = True 
isSubsequenceOf _ [] = False 
isSubsequenceOf (x:xs) (y:ys) 
   | x == y    = isSubsequenceOf xs ys 
   | otherwise = isSubsequenceOf (x:xs) ys 

-- 2 -- 
data BTree a = Empty | Node a (BTree a) (BTree a)

-- a -- 
lookupAP :: Ord a => a -> BTree (a, b) -> Maybe b
lookupAP _ Empty = Nothing 
lookupAP x (Node (a,b) l r)
    | x < a  = lookupAP x l 
    | x == a = Just b 
    | x > a  = lookupAP x r

-- b -- 
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
zipWithBT f (Node e1 l1 r1) (Node e2 l2 r2) = Node (f e1 e2) (zipWithBT f l1 l2) (zipWithBT f r1 r2)
zipWithBT _ _ _ = Empty

-- 3 -- 
isDigit :: Char -> Bool 
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool 
isAlpha c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

digitAlpha :: String -> (String, String)
digitAlpha = foldr (\c (nums, wørt) -> if isDigit c then (c : nums, wørt)
                                                    else if isAlpha c then (nums, c : wørt)
                                                                      else (nums, wørt)) ("","")

-- 4 -- 
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a) 
-- a --
firstSeq :: Seq a -> a 
firstSeq (Cons x _) = x 
firstSeq (App Nil _seq) = firstSeq _seq 
firstSeq (App _seq _)   = firstSeq _seq 

-- b -- 
drop1 :: Seq a -> Seq a 
drop1 (Nil)          = Nil 
drop1 (Cons x _seq)  = _seq 
drop1 (App Nil _seq) = drop1 _seq
drop1 (App _seq Nil) = drop1 _seq 
drop1 (App _seq sseq)= App (drop1 _seq) sseq 

dropSeq :: Int -> Seq a -> Seq a 
dropSeq n _seq | n <= 0 = _seq 
dropSeq n _seq = dropSeq (n-1) (drop1 _seq)

-- c -- 
toLst :: Seq a -> [a]
toLst  Nil          = []
toLst (Cons x _seq) = x : toLst _seq 
toLst (App s1 s2)   = toLst s1 ++ toLst s2 

stringify :: Show a => [a] -> String 
stringify l = "<<" ++ intercalate "," (map show l) ++ ">>"

instance (Show a) => Show (Seq a) where 
    show = stringify . toLst 

instance (Eq a) => Eq (Seq a) where
    (==) seq1 seq2 = (==) (toLst seq1) (toLst seq2) 

-- 5 -- 
type Mat a = [[a]]
mat = [[6,7,2], [1,5,9], [8,3,4]] :: Mat Int

-- a -- 
getElem :: Mat a -> IO a
getElem mat = do let m = length mat 
                 let n = (length . head) mat 
                 i <- randomRIO (0, m-1)
                 j <- randomRIO (0, n-1)
                 return ((mat !! i) !! j)

-- b --
trace :: Mat Int -> Int 
trace = foldl(\acc (l, indx) -> acc + (l !! indx)) 0 . flip zip [0..]

antitrace :: Mat Int -> Int 
antitrace = foldl(\acc (l, indx) -> acc + (l !! indx)) 0 . reverse . flip zip [0..]

magic :: Mat Int -> Bool
magic m = 
    (==) 1 ((length . nub . map sum) m) && (==) 1 ((length . nub . map sum . transpose) m)
    && (==) (trace m) v 
    && (==) (antitrace m) v
    && (==) v v'
    where v = (head . map sum) m 
          v' = (head . map sum . transpose) m  
