-- Fibonacci Modified
-- https://www.hackerrank.com/challenges/fibonacci-modified

-- Imperative fibonacci finder in the ST monad
-- Of course it's easier to do this with the canonical zipWith implementation

import Control.Monad
import Control.Monad.ST.Strict
import Control.Applicative
import Data.STRef

runFib :: Integer -> Integer -> Int -> Integer
runFib a1 b1 k = runST $ do
    a <- newSTRef a1
    b <- newSTRef b1
    r <- newSTRef 0

    replicateM_ (k - 2) $ do
        a' <- readSTRef a
        b' <- readSTRef b

        writeSTRef r (b'^2 + a')

        writeSTRef a b'
        writeSTRef b =<< readSTRef r

    readSTRef r

main :: IO()
main = do
    [a,b,n] <- map read . words <$> getLine
    print (runFib a b (fromInteger n))
