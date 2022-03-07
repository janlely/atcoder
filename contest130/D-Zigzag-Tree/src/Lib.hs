{-# LANGUAGE BangPatterns#-}
module Lib
    ( someFunc
    ) where

import Control.Monad (replicateM, forM_)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.ST ( ST, runST ) 
import Debug.Trace (trace)


data CountTree = CountTree
    { count_ :: Int
    , childs_ :: [CountTree]
    } deriving Show

process :: CountTree -> VU.Vector Int
process (CountTree 1 []) = VU.singleton 1
process (CountTree n childs) = VU.fromList . fst . VU.foldl' foldFunc ([0],0) $ getResult childs
    where foldFunc (ls, v1) v2 = let v = (v1 + v2) `mod` 998244353 in (v:ls, v)
          getResult chs = let !cs = map process chs
                           in snd . L.foldl1' combine $ zip (map count_ chs) cs

combine :: (Int, VU.Vector Int) -> (Int, VU.Vector Int) -> (Int, VU.Vector Int)
combine (c1, v1) (c2, v2) = (c1+c2, runST combine')
    where comb :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
          comb (n, i, pi) (m, j, pj) = L.foldl' (\res k ->
              let !x = cm (i-1) (i+j+k-1) * cm (n-i) (m+n-i-j-k) * pi * pj
               in (i+j+k-1, x `mod` 998244353) : res) [] [0,1..m-j]
          toPair = filter ((>0) . snd) . VU.toList . VU.indexed
          combine' :: ST s (VU.Vector Int)
          combine' = do
              v <- VUM.replicate (c1+c2) 0
              forM_ [(i1, p1, i2, p2)| (i1, p1) <-  toPair v1, (i2, p2) <- toPair v2] $ \(a,b,c,d) -> do
                  let cs1 = comb (c1, a+1, b) (c2, c+1, d)
                      cs2 = comb (c2, c+1, d) (c1, a+1, b)
                  forM_ cs1 $ \(i, p) -> do
                      VUM.modify v ((`mod` 998244353) . (+ p)) i
                  forM_ cs2 $ \(i, p) -> do
                      VUM.modify v ((`mod` 998244353) . (+ p)) i
              VU.unsafeFreeze v

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
    print $ 2 * VU.foldl' (\x y -> (x+y) `mod` 998244353) 0 (process ct) `mod` 998244353 
