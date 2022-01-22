module Lib
    ( someFunc
    ) where

-- import Control.Monad
-- import Debug.Trace (trace)
-- import qualified Data.ByteString.Char8 as B

-- readInt :: B.ByteString -> Int
-- readInt s = let (i,_) = fromJust $ B.readInt s in i

-- solve :: Int -> String -> Int
-- solve l str = (sum . fmap (\n -> n * (n - 1) `div` 2)) grps
    -- where grps = solve' (head str) (tail str) 1 []
          -- solve' :: Char -> String -> Int -> [Int] -> [Int]
          -- solve' _ [] l res = (if l > 1 then (l:res) else res)
          -- solve' a (b:chs) l res
            -- | a == b = solve' b chs (l+1) res
            -- | a /= b = solve' b chs 1 (if l > 1 then (l:res) else res)

solve :: String -> Int
solve s = sum . map comb . trd $ foldl f ((head s, 1, [])) (tail s)
    where f (s1, c, res) s2 = if s1 == s2
                                 then (s2, succ c, res)
                                 else (s2, 1, c:res)
          trd (a,b,c) = b:c
          comb n = n * (n - 1) `div` 2


someFunc = do
    _ <- (read::String -> Int) <$> getLine 
    str <- getLine
    print $ solve str
    


