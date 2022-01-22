module Lib
    ( someFunc
    ) where

import Data.Char (digitToInt, intToDigit)
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad (fmap, join, forM_)
import Data.Bifunctor (bimap)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isJust)

type Path = (Int, Int)


resort :: [Path] -> [Path]
resort = uncurry (flip (++)) . span ((== 9) . uncurry (+))


isCurrect :: [Path] -> Bool
isCurrect [] = False
isCurrect ((a,b):abs) = (a + b > 9) || isCurrect abs

adjustPath :: [Int] -> [Int] -> [Path] -> ([Path], [Int], [Int])
adjustPath la lb [] = ([], la, lb)
adjustPath la lb ps@((a,b):abs)
  | not (null lb1) = ((a, last lb1):abs, la, init lb1 ++ [b] ++ lb2) 
  | otherwise      = if null abs
                       then (ps, la, lb)
                       else let (x,y) = last abs
                             in ((a,y):init abs ++ [(x,b)], la, lb)
  where (lb1, lb2) = span (> b) lb

solve :: [Int] -> [Int] -> ([Path], [Int], [Int]) -> ([Path], [Int], [Int])
solve [] lb (ps, la', lb') = (ps, la', reverse lb ++ lb')
solve la [] (ps, la', lb') = (ps, la ++ la', lb')
solve la@(a:as) (b:bs) (ps, la', lb')
  | a + b >= 9 = solve as bs ((a,b):ps, la', lb')
  | otherwise  = solve la bs (ps, la', b:lb')

solve' :: [Int] -> [Int] -> ([Path], [Int], [Int])
solve' la lb
  | length la <= length lb = let (res, la', lb') = solve (reverse la) lb ([], [], [])
                              in if isCurrect res
                                   then (res, la', lb')
                                   else adjustPath la' lb' res
  | otherwise              = let (res, la', lb') = solve (reverse lb) la ([], [], [])
                              in if isCurrect res
                                   then bottomUp (res, la', lb')
                                   else bottomUp $ adjustPath la' lb' res
  where bottomUp (a, b, c) = (change a, c, b) 
        change x = let (a,b) = unzip x in zip b a

formatResult :: [Path] -> [Int] -> [Int] -> (String, String)
formatResult ps la lb = (intToDigit <$> psa ++ la, intToDigit <$> psb ++ lb)
  where (psa, psb) = unzip ps

someFunc = do
  la <- L.sort . fmap digitToInt <$> getLine
  lb <- L.sort . fmap digitToInt <$> getLine
  let (ps, la', lb') = solve' la lb
      (la1, lb1) = formatResult (resort $ reverse ps) la' lb'
  putStrLn (reverse la1)
  putStrLn (reverse lb1)


