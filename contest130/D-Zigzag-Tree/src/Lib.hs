module Lib
    ( someFunc
    ) where

import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as VU
import qualified  Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ord (comparing)


data CountTree = CountTree
    { count_ :: Int
    , childs_ :: [CountTree]
    } deriving Show


solve :: CountTree -> VU.Vector Int
solve (CountTree 1 []) = VU.fromList [0,1]
solve (CountTree n childs) = VU.fromList
                             . (0:)
                             . reverse
                             . map (snd . bigToSmall . L.foldl1 merge)
                             . L.groupBy (equalBy fst)
                             . L.sortBy (comparing fst)
                             . concatMap (L.unfoldr extend) 
                             $ getResult childs
    where extend (x,y)
            | x + 1 <= n = Just ((x+1, y), (x+1, y))
            | otherwise = Nothing 
          bigToSmall (x, y) = (n + 1 - x, y)
          getResult chs = snd . L.foldl1 combine $ zip (map count_ chs) (map (tail . VU.toList . VU.indexed . solve) chs)
    
          
merge :: (Int, Int) -> (Int, Int) -> (Int, Int)
merge (a,b) (c,d)
  | a == c = (a, b+d)
  | otherwise = error "invalid input" 
    
combine :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
combine (c1,l1) (c2,l2) = (c1 + c2, combs)
    where combs = map (L.foldl1 merge)
              . L.groupBy (equalBy fst) . concat $ do
              (i1, p1) <- l1
              (i2, p2) <- l2
              return $ comb (c1, i1, p1) (c2, i2, p2) ++ comb (c2, i2, p2) (c1, i1, p1)
          comb :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
          comb (n, i, pi) (m, j, pj) = L.foldl (\res k -> (i+j+k, cm (i-1) (i+j+k-1) * cm (n-i) (m+n-i-j-k) * pi * pj) : res) [] [0,1..m-j]

          
equalBy :: (Eq b) => (a -> b) -> a -> a -> Bool
equalBy f a b = f a == f b

cm :: Int -> Int -> Int
cm 0 _ = 1
cm _ 0 = 1
cm a b = b * cm (a-1) (b-1) `div` a

perm :: [a] -> [[a]]
perm []     = return []
perm (x:xs) = perm xs >>= ins x
    where
    ins :: a -> [a] -> [[a]]
    ins x []     = [[x]]
    ins x (y:ys) = (x:y:ys) : map (y:) (ins x ys)

buildCountTree :: Int -> M.Map Int (S.Set Int) -> CountTree
buildCountTree root src
    | not (M.member root src) = CountTree 1 []
    | otherwise = CountTree (sum (map count_ childTrees) + 1) childTrees
    where childNodes = S.toList $ src M.! root 
          newSrc = M.filter (not . S.null) . M.map (S.delete root) $ src
          childTrees = map (`buildCountTree` newSrc) childNodes

someFunc :: IO ()
someFunc = do
    n <- read <$> getLine
    edges <- replicateM (n-1) $ do
        [a,b] <- fmap read . words <$> getLine
        return (a,b)
    let m = L.foldl foldFunc M.empty edges
        foldFunc a (b,c) = M.unionsWith S.union [M.singleton b (S.singleton c), M.singleton c (S.singleton b), a]
        ct = buildCountTree 1 m 
    print $ 2 * VU.sum (solve ct)
