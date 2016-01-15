-- print the nth pentagonal number
import Control.Monad

main = do
    t <- readLn
    replicateM_ t $ do
        n <- readLn
        print $ (3*n^2 - n) `div` 2
