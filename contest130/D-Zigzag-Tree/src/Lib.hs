{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    ) where

import Control.Monad (replicateM_,forM_,when,unless)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import Control.Parallel.Strategies ( parMap, rpar )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, tryPutMVar )

modulo :: Integral a => a -> a
modulo = (`mod` 998244353)

readInt :: B.ByteString -> Int
readInt s = let (i,_) = fromJust $ B.readInt s in i
{-# INLINE readInt #-}

mm :: Int -> Int -> Int
mm a b = modulo (a * b)

mp :: Int -> Int -> Int
mp a b = modulo (a + b)

someFunc :: IO ()
someFunc = do
    n <- readInt <$> B.getLine
    edgeList <- VM.replicate (n+1) []
    replicateM_ (n-1) $ do
        [a,b] <- fmap readInt . B.words <$> B.getLine
        VM.modify edgeList (b:) a
        VM.modify edgeList (a:) b
    edges <- V.unsafeFreeze edgeList
    combTable <- VM.replicate (n+1) (VU.empty::VU.Vector Int)
    -- locks <- VM.new (n+1)
    -- locks2 <- VM.new (n+1)
    -- forM_ [1,2..n] $ \i -> do
    --     lock <- newEmptyMVar 
    --     VM.write locks i lock
    -- forM_ [1,2..n] $ \i -> do
    --     lock <- newEmptyMVar 
    --     VM.write locks2 i lock
    let compute :: Int -> IO ()
        compute i = do
            -- lock <- VM.read locks i
            -- res <- tryPutMVar lock 1
            -- when res $ do
            combv <- VUM.replicate (i+1) 0
            let i' = fromIntegral i
                idx = [2,3..i' `div` 2]
                combs = zip [2,3..i `div` 2] . tail . scanl (\v k -> v * (i'-k+1) `div` k) i' $ idx
            forM_ combs $ \(ix, v) -> do
                VUM.write combv ix (fromIntegral $ modulo v)
            combv' <- VU.unsafeFreeze combv
            VM.write combTable i combv'
            -- lock <- VM.read locks2 i
            -- putMVar lock 1
        comb :: Int -> Int -> IO Int
        comb i j
          | i == j || i == 0 || j == 0 = return 1
          | i == 1 = return j
          | 2*i > j = comb (j-i) j
          | otherwise = do
            -- compute j
            ct <- VM.read combTable j
            when (VU.null ct) (compute j)
            ct' <- VM.read combTable j
            -- lock <- VM.read locks2 j
            -- readMVar lock
            -- ct <- VM.read combTable j
            return $ ct' VU.! i
        combine :: IO (Int, VU.Vector Int) -> IO (Int, VU.Vector Int) -> IO (Int, VU.Vector Int)
        combine c1v1 c2v2 = do
            (c1, v1) <- c1v1
            (c2, v2) <- c2v2
            let v1' = VU.scanl1 mp v1
                v2' = VU.scanl1 mp v2
            (c1+c2,) <$> do
                  v <- VUM.replicate (c1+c2) 0
                  forM_ (toPair v1) $ \(i,p) -> do
                      forM_ [1,2..c2] $ \k -> do
                          comb1 <- comb i (i+k)
                          comb2 <- comb (c1-i-1) (c1+c2-i-1-k)
                          let p' = p `mm` (v2' VU.! (k-1)) `mm` comb1 `mm` comb2
                          VUM.modify v (`mp` p') (i+k)
                  forM_ (toPair v2) $ \(i,p) -> do
                      forM_ [1,2..c1] $ \k -> do
                          comb1 <- comb i (i+k)
                          comb2 <- comb (c2-i-1) (c1+c2-i-1-k)
                          let p' = p `mm` (v1' VU.! (k-1)) `mm` comb1 `mm` comb2
                          VUM.modify v (`mp` p') (i+k)
                  VU.unsafeFreeze v
          where toPair = filter ((>0) . snd) . VU.toList . VU.takeWhile ((>0) . snd) . VU.dropWhile ((== 0) . snd) . VU.indexed
        solve :: Int -> Int -> IO (Int, VU.Vector Int)
        solve parent node
          | null childNodes = return  (1, VU.singleton 1)
          | otherwise = do
            --  (count, combines) <- foldl1 combine . parMap rpar (solve node) $ childNodes
             (count, combines) <- foldl1 combine . map (solve node) $ childNodes
             return (count+1, VU.fromList . fst . VU.foldl' foldFunc ([0],0) $ combines)
          where foldFunc (ls, v1) v2 = let v = v1 `mp` v2 in (v:ls, v)
                childNodes = filter (/= parent) $ edges V.! node
    (c, v) <- solve 0 1
    print $ (2 `mm`) . VU.sum $ v
