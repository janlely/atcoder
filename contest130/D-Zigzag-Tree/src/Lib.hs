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

modulo :: Int -> Int
modulo = (`mod` 998244353)

process :: CountTree -> VU.Vector Int
process (CountTree 1 []) = VU.singleton 1
process (CountTree n childs) = VU.fromList . fst . VU.foldl' foldFunc ([0],0) $ getResult childs
    where foldFunc (ls, v1) v2 = let v = modulo (v1 + v2) in (v:ls, v)
          getResult chs = let cs = map process chs
                           in snd . L.foldl1' combine $ zip (map count_ chs) cs

combine :: (Int, VU.Vector Int) -> (Int, VU.Vector Int) -> (Int, VU.Vector Int)
combine (c1, v1) (c2, v2) = (c1+c2, runST combine')
    where toPair = filter ((>0) . snd) . VU.toList . VU.takeWhile ((>0) . snd) . VU.dropWhile ((== 0) . snd) . VU.indexed
          v1' = VU.scanl1 (+) v1
          v2' = VU.scanl1 (+) v2
          comb :: (Int, Int, Int) -> (Int, VU.Vector Int) -> [(Int, Int)]
          comb (n, i, p) (m, v) = L.foldl' (\res k ->(i+k-1, p * (v VU.! (k-1)) * cm (i-1) (i-1+k) * cm (n-i) (m+n-i-k)) : res) [] [1,2..m]
          combine' :: ST s (VU.Vector Int)
          combine' = do
              v <- VUM.replicate (c1+c2) 0
              forM_ (toPair v1) $ \(i,p) -> do
                  let !cs1 = comb (c1, i+1, p) (c2, v2')
                  forM_ cs1 $ \(i', p') -> do
                      VUM.modify v (modulo . (+ p')) i'
              forM_ (toPair v2) $ \(i,p) -> do
                  let !cs2 = comb (c2, i+1, p) (c1, v1')
                  forM_ cs2 $ \(i', p') -> do
                      VUM.modify v (modulo . (+ p')) i'
              VU.unsafeFreeze v

cm :: Int -> Int -> Int
cm 0 _ = 1
cm _ 0 = 1
cm a b = b * cm (a-1) (b-1) `div` a

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
        !ct = buildCountTree 1 m 
    print . modulo $ 2 * VU.foldl' (\x y -> modulo (x+y)) 0 (process ct)
