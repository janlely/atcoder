import Control.Monad (forM_)
main = do
    n <- read <$> getLine
    print n
    forM_ [1,2..n-1] (\i -> do
        putStr "1 "
        print (i+1))


