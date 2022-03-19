{-# LANGUAGE BangPatterns#-}
module Lib
    ( someFunc
    ) where

import Control.Monad (replicateM, forM_, sequence_)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (fromJust)
import Control.Monad.ST ( ST, runST ) 
import qualified Data.Array as A
import Debug.Trace (trace)
import qualified Data.ByteString.Char8 as B
import Data.Bifunctor (second)

data CountTree = CountTree
    { count_ :: Int
    , value_ :: Int
    , childs_ :: [CountTree]
    } deriving Show

modulo :: Int -> Int
modulo = (`mod` 998244353)

readInt :: B.ByteString -> Int
readInt s = let (i,_) = fromJust $ B.readInt s in i
{-# INLINE readInt #-}

process :: Int -> VU.Vector Int -> CountTree -> VU.Vector Int
process c mt (CountTree 1 i []) = VU.singleton 1 
process c mt (CountTree n i childs) = combineChilds childSolutions
    where combineChilds = VU.fromList . fst . VU.foldl' foldFunc ([0],0)
          foldFunc (ls, v1) v2 = let v = v1 `mp` v2 in (v:ls, v)
          childSolutions = let cs = map (process c mt) childs
                           in snd . L.foldl1' (combine c mt) $ zip (map count_ childs) cs

combine :: Int -> VU.Vector Int -> (Int, VU.Vector Int) -> (Int, VU.Vector Int) -> (Int, VU.Vector Int)
combine c mt (c1, v1) (c2, v2) = (c1+c2, runST combine')
    where toPair = filter ((>0) . snd) . VU.toList . VU.takeWhile ((>0) . snd) . VU.dropWhile ((== 0) . snd) . VU.indexed
          !v1' = VU.scanl1 mp v1
          !v2' = VU.scanl1 mp v2
          merge :: (Int, Int, Int) -> (Int, VU.Vector Int) -> [(Int, Int)]
          merge (n, i, p) (m, v) = L.foldl' (\res k ->(i+k-1, let pp = p `mm` (v VU.! (k-1)) `mm` cm c mt (i-1) (i-1+k) `mm` cm c mt (n-i) (m+n-i-k) in pp) : res) [] [1,2..m]
          combine' :: ST s (VU.Vector Int)
          combine' = do
              v <- VUM.replicate (c1+c2) 0
              forM_ (toPair v1) $ \(i,p) -> do
                  let !cs1 = merge (c1, i+1, p) (c2, v2')
                  forM_ cs1 $ \(i', p') -> do
                      VUM.modify v (`mp` p') i'
              forM_ (toPair v2) $ \(i,p) -> do
                  let !cs2 = merge (c2, i+1, p) (c1, v1')
                  forM_ cs2 $ \(i', p') -> do
                      VUM.modify v (`mp` p') i'
              VU.unsafeFreeze v

mm :: Int -> Int -> Int
mm a b = modulo (a * b)

mp :: Int -> Int -> Int
mp a b = modulo (a + b)

-- calculate combination
cm :: Int -> VU.Vector Int -> Int -> Int -> Int
cm n mt i j
  | i == 0 || j == 0 || i == j = 1
  | 2*i > j = mt VU.! toIndex n (j-i) j
  | otherwise = mt VU.! toIndex n i j

toIndex :: Int -> Int -> Int -> Int
toIndex n i j = (i - 1) * n + j

mkCombTable :: Int -> VU.Vector Int 
mkCombTable n = VU.update (VU.replicate (n*n) 0) . VU.fromList . map (second (fromIntegral . (`mod` 998244353))) . fst . L.foldl (\(res, v) (i,j) -> let v' = comb i j v in ((toIndex n j i, v'):res, v')) ([], 0) $ [(x,y)|x <- [1,2..n], y <- [1,2..(x `div` 2)]]
    where comb :: Int -> Int -> Integer -> Integer
          comb i j v
            | j == 1 = fromIntegral i 
            | otherwise = v * fromIntegral (i-j+1) `div` fromIntegral j


buildCountTree :: Int -> M.Map Int (S.Set Int) -> CountTree
buildCountTree root src
    | not (M.member root src) = CountTree 1 root []
    | otherwise = CountTree (sum (map count_ childTrees) + 1) root childTrees
    where childNodes = S.toList $ src M.! root 
          newSrc = M.filter (not . S.null) . M.map (S.delete root) $ src
          childTrees = map (`buildCountTree` newSrc) childNodes

printCountTree :: Int -> CountTree -> IO ()
printCountTree n (CountTree 1 v []) = do
    sequence_ $ putChar  <$> replicate n ' '
    putChar '|'
    putStr "___"
    print v
printCountTree n (CountTree c v chs) = do
    sequence_ $ putChar  <$> replicate n ' '
    putChar '|'
    putStr "___"
    print v
    sequence_ $ printCountTree (n+4) <$> chs
    

someFunc :: IO ()
someFunc = do
    n <- readInt <$> B.getLine
    edges <- replicateM (n-1) $ do
        [a,b] <- fmap readInt . B.words <$> B.getLine
        return (a,b)
    let m = L.foldl foldFunc M.empty edges
        foldFunc a (b,c) = M.unionsWith S.union [M.singleton b (S.singleton c), M.singleton c (S.singleton b), a]
        !ct = buildCountTree 1 m 
        !combTable = mkCombTable n
    print $ 2 `mm` VU.foldl' mp 0 (process n combTable ct)
