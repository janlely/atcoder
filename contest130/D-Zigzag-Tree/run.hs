import qualified Data.Vector.Unboxed as VU
import qualified  Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

combine :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
combine (c1,l1) (c2,l2) = (c1 + c2, combs)
    where combs = map (L.foldl1 merge)
              . L.groupBy (equalBy fst) $ do
              (i1, p1) <- l1
              (i2, p2) <- l2
              let x = permutation i1 (i1 + i2)
                  y = permutation (c1 - i1) (c1 + c2 - i1 - i2) 
              return (i1 + i2,  x * y * p1 * p2)
permutation :: Int -> Int -> Int
permutation 1 b = b
permutation a b = b * permutation (a-1) (b-1) `div` a

equalBy :: (Eq b) => (a -> b) -> a -> a -> Bool
equalBy f a b = f a == f b

merge :: (Int, Int) -> (Int, Int) -> (Int, Int)
merge (a,b) (c,d)
  | a == c = (a, b+d)
  | otherwise = error "invalid input" 

extend (x,y)
  | x < 4 = Just ((x, y), (x+1, y))
  | otherwise = Nothing 
bigToSmall (x, y) = (4 - x, y)

res = [[(1,1)], [(1,1)], [(1,1)]]
counts = [1,1,1]

main = print . snd . L.foldl1 combine $ zip counts res 
