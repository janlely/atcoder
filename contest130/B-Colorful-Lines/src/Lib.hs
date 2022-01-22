module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Char8 as B
import Data.HashSet as HS
import Data.List as L
import Control.Monad
import Data.Maybe
import qualified  Data.Vector.Unboxed as VU


readInt :: B.ByteString -> Int
readInt s = let (i,_) = fromJust $ B.readInt s in i
{-# INLINE readInt #-}

unsafeToTriple :: [Int] -> (Int, Int, Int)
unsafeToTriple (a:b:c:_) = (a, pred b, pred c)
{-# INLINE unsafeToTriple #-}

-- version 1
-- someFunc = do
    -- [h,w,c,q] <- fmap readInt . B.words <$> B.getLine
    -- let f :: ((S.HashSet Int, S.HashSet Int), [(Int, Int)]) -> (Int, Int, Int) -> ((S.HashSet Int, S.HashSet Int), [(Int, Int)])
        -- f ((rs, cs), res) (1, num, color)
          -- | S.member num rs = ((rs, cs), res)
          -- | otherwise       = ((S.insert num rs, cs), (color-1, w - S.size cs):res)
        -- f ((rs, cs), res) (_, num, color)
          -- | S.member num cs = ((rs, cs), res)
          -- | otherwise       = ((rs, S.insert num cs), (color-1, h - S.size rs):res)
        -- g = unwords . fmap show . V.toList . V.unsafeAccum (+) (V.replicate c 0) . snd . L.foldl f ((S.empty, S.empty), []) . L.reverse
    -- res <- g . fmap (unsafeToTriple . fmap readInt . B.words) . B.lines <$> B.getContents
    -- putStrLn res

-- version 2
-- someFunc = do
    -- [h,w,c,q] <- fmap readInt . B.words <$> B.getLine
    -- let f :: ((HS.HashSet Int, Int), (HS.HashSet Int, Int)) -> (Int, Int, Int) -> (((HS.HashSet Int, Int), (HS.HashSet Int, Int)), (Int, Int))
        -- f rc@((rs, rw), rc2@(_, cw)) (1, num, color)
          -- | HS.member num rs = (rc, (color, 0))
          -- | otherwise       = (((HS.insert num rs, pred rw), rc2), (color, cw))
        -- f rc@(rc1@(_, rw), (cs, cw)) (_, num, color)
          -- | HS.member num cs = (rc, (color, 0))
          -- | otherwise       = ((rc1, (HS.insert num cs, pred cw)), (color, rw))
        -- g = VU.unsafeAccum (+) (VU.replicate c 0) . snd . L.mapAccumL f ((HS.empty, h), (HS.empty, w))
    -- res <- L.reverse . fmap (unsafeToTriple . fmap readInt . B.words) . B.lines <$> B.getContents
    -- res `seq` (putStrLn . unwords . fmap show . VU.toList . g $ res)

    
someFunc = do
    [h,w,c,q] <- fmap readInt . B.words <$> B.getLine
    let f :: ((HS.HashSet Int, Int), (HS.HashSet Int, Int)) -> (Int, Int, Int) -> (((HS.HashSet Int, Int), (HS.HashSet Int, Int)), (Int, Int))
        f rc@((rs, rw), rc2@(_, cw)) (1, num, color)
          | HS.member num rs = (rc, (color, 0))
          | otherwise       = (((HS.insert num rs, pred rw), rc2), (color, cw))
        f rc@(rc1@(_, rw), (cs, cw)) (_, num, color)
          | HS.member num cs = (rc, (color, 0))
          | otherwise       = ((rc1, (HS.insert num cs, pred cw)), (color, rw))
        g = VU.unsafeAccum (+) (VU.replicate c 0) . snd . L.mapAccumL f ((HS.empty, h), (HS.empty, w))
    res <- VU.toList . VU.reverse . VU.fromListN q . fmap (unsafeToTriple . fmap readInt . B.words) . B.lines <$> B.getContents
    putStrLn . unwords . fmap show . VU.toList . g $ res

-- someFunc = do
    -- [h,w,k,q] <- fmap readInt . B.words <$> B.getLine
    -- let f :: [(Bool, Int, Int)] -> VU.Vector Int
        -- f = VU.unsafeAccum (+) (VU.replicate k 0) . snd . L.mapAccumL step ((h, HS.empty), (w, HS.empty))
            -- where step :: ((Int, HashSet Int), (Int, HashSet Int)) -> (Bool, Int, Int) -> (((Int, HashSet Int), (Int, HashSet Int)), (Int, Int))
                  -- step t@((lH, sH), pW@(lW, _)) (False, n, c)
                    -- | HS.member n sH = (t, (c, 0))
                    -- | otherwise      = (((pred lH, HS.insert n sH), pW), (c, lW))
                  -- step t@(pH@(lH, _), (lW, sW)) (_, n, c)
                    -- | HS.member n sW = (t, (c, 0))
                    -- | otherwise      = ((pH, (pred lW, HS.insert n sW)), (c, lH))
    -- tncs <- VU.toList . VU.reverse . VU.fromListN q . fmap (unsafeToTriple . fmap readInt . B.words) . B.lines <$> B.getContents
    -- putStrLn . unwords . fmap show . VU.toList $ f tncs
    --
-- someFunc = do
    -- [h,w,k,q] <- fmap readInt . B.words <$> B.getLine
    -- let f :: [(Bool, Int, Int)] -> VU.Vector Int
        -- f = VU.unsafeAccum (+) (VU.replicate k 0) . snd . L.mapAccumL step (HS.empty, HS.empty)
            -- where step :: (HashSet Int, HashSet Int) -> (Bool, Int, Int) -> ((HashSet Int, HashSet Int), (Int, Int))
                  -- step (rs, cs) (False, n, c)
                    -- | HS.member n rs = ((rs, cs), (c, 0))
                    -- | otherwise      = ((HS.insert n rs, cs), (c, w - HS.size cs))
                  -- step (rs, cs) (_, n, c)
                    -- | HS.member n cs = ((rs, cs), (c, 0))
                    -- | otherwise      = ((rs, HS.insert n cs), (c, h - HS.size rs))
    -- tncs <- VU.toList . VU.reverse . VU.fromListN q . fmap (unsafeToTriple . fmap readInt . B.words) . B.lines <$> B.getContents
    -- putStrLn . unwords . fmap show . VU.toList $ f tncs
