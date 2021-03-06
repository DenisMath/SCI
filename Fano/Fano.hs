module Fano (
  freqConvert,
  getCode,
  simpleCode,
  decodeFano )
where

import Data.List
import Data.Ord

listEl :: Eq t => [t] -> t -> [t]
listEl [] m = []
listEl a@(ha:ta) m = if ha == m
  then [ha]
  else [ha] ++ listEl ta m

minimInd :: Ord a => [a] -> Int
minimInd xs = length (listEl xs (minimum xs)) - 1

sumList :: Num a => [a] -> [a]
sumList [] = []
sumList a@(ha:ta) = if length a == 1
    then [head a]
    else [sum a] ++ sumList ta


diffList :: Num b => [b] -> [b]
diffList a = let b@(hb:tb)=sumList a
    in map (\x -> abs(2*x - hb)) b


divideInd :: (Ord a, Num a) => [a] -> Int
divideInd a = minimInd $ diffList a

data Tree a = EmptyTree | TreeTag a |  Node (Tree a) (Tree a) deriving (Show, Eq)

eTree :: (Ord b, Ord a, Num a) => [(a, b)] -> Tree (a, b)
eTree [] = EmptyTree 
eTree ain =
  let a = sort ain in
      if length a == 1
      then TreeTag (head a)
      else
          let n = divideInd (map fst a)
              in Node (eTree(take n a)) (eTree(drop n a))

codeList :: Tree (a1, a2) -> [(a2, [Char])]
codeList (Node (TreeTag x) (TreeTag y)) = [(snd x, "0"), (snd y, "1")]
codeList (Node (aL) (TreeTag y)) = (map (fmap ((++) "0")) (codeList aL)) ++ [(snd y, "1")]
codeList (Node (TreeTag x) (aR)) = [(snd x, "0")] ++ (map (fmap ((++) "1")) (codeList aR))
codeList (Node (aL) (aR)) = (map (fmap ((++) "0")) (codeList aL)) ++ (map (fmap ((++) "1")) (codeList aR))
codelist _ = []

buildFreqList :: (Num a1, Ord a2) => [a2] -> [(a1, a2)]
buildFreqList a = map (\x -> (fromIntegral(length x), head x))(group $ sort a)

unJustCh (Just x) = x

encodeFanoS :: Ord a => [a] -> [Char]
encodeFanoS a = let c = simpleCode a in
  foldr (\acc t -> acc ++ t) "" $ map (\x -> unJustCh(lookup x c)) a

chng :: [(b, a)] -> [(a, b)]
chng a = map (\(x,y) -> (y,x)) a

--------------------------------------------------------------------------------

freqConvert :: Fractional a => [(a, b)] -> [(a, b)]
freqConvert a = map (\x -> (fst x/ sum (map fst a), snd x)) a

getCode ::  (Ord a2, Ord a1, Fractional a1) => [(a1, a2)] -> [(a2, [Char])]
getCode = codeList . eTree .freqConvert

simpleCode :: Ord a2 => [a2] -> [(a2, [Char])]
simpleCode  = getCode . freqConvert . buildFreqList

decodeFanoM fr [] = []
decodeFanoM fr code =  let l = (head $ filter (\x -> isPrefixOf x code) (map snd fr))
                             in
                               [(lookup l $ chng fr)] ++ decodeFanoM fr (drop (length l) code)

decodeFano fr code = foldl (\acc x -> acc ++ [unJustCh x]) [] (decodeFanoM fr code)                          






                     
