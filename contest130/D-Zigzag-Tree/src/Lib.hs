module Lib
    ( someFunc
    ) where

import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as VU
import qualified  Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ord (comparing)
import Debug.Trace (trace)


data CountTree = CountTree
    { count_ :: Int
    , childs_ :: [CountTree]
    } deriving Show


solve :: CountTree -> VU.Vector Int
solve (CountTree 1 []) = VU.fromList [0,1]
solve (CountTree n childs) = trace (show res ++ show counts) VU.fromList . (0:) . map (snd . bigToSmall . L.foldl1 merge) . L.groupBy (equalBy fst) . concatMap (L.unfoldr extend) . snd . L.foldl1 combine $ zip counts res 
    where counts = map count_ childs
          res = map (VU.toList . VU.tail . VU.indexed . solve) childs
          extend (x,y)
            | x + 1 <= n = Just ((x+1, y), (x+1, y))
            | otherwise = Nothing 
          bigToSmall (x, y) = (n + 1 - x, y)

merge :: (Int, Int) -> (Int, Int) -> (Int, Int)
merge (a,b) (c,d)
  | a == c = (a, b+d)
  | otherwise = error "invalid input" 
    
combine :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
combine (c1,l1) (c2,l2) = (c1 + c2, combs)
    where combs = map (L.foldl1 merge)
              . L.groupBy (equalBy fst) $ do
              (i1, p1) <- l1
              (i2, p2) <- l2
              let x = permutation i1 (i1 + i2)
                  y = permutation (c1 - i1) (c1 + c2 - i1 - i2) 
              return (i1 + i2,  x * y * p1 * p2)
          
equalBy :: (Eq b) => (a -> b) -> a -> a -> Bool
equalBy f a b = f a == f b

permutation :: Int -> Int -> Int
permutation 1 b = b
permutation 0 _ = 1
permutation _ 0 = 1
permutation a b = b * permutation (a-1) (b-1) `div` a

buildCountTree :: Int -> M.Map Int (S.Set Int) -> CountTree
buildCountTree root src
    | not (M.member root src) = CountTree 1 []
    | otherwise = CountTree (sum (map count_ childTrees) + 1) childTrees
    where childNodes = S.toList $ src M.! root 
          newSrc = M.filter (not . S.null) . M.map (S.delete root) $ src
          childTrees = map (`buildCountTree` newSrc) childNodes
printCountTree :: CountTree -> String
printCountTree = L.intercalate "\n" . printTree 0
    where printTree :: Int -> CountTree -> [String] 
          printTree i (CountTree c []) = [replicate (i*4) ' ' ++ show c]
          printTree i (CountTree c childs) = (replicate (i*4) ' ' ++ show c) : concatMap (printTree (i+1)) childs

someFunc :: IO ()
someFunc = do
    n <- read <$> getLine
    edges <- replicateM (n-1) $ do
        [a,b] <- fmap read . words <$> getLine
        return (a,b)
    let m = L.foldl foldFunc M.empty edges
        foldFunc a (b,c) = M.unionsWith S.union [M.singleton b (S.singleton c), M.singleton c (S.singleton b), a]
        ct = buildCountTree 1 m 
    putStrLn . printCountTree $ ct
    print $ 2 * VU.sum (solve ct)
