import Data.List
import Control.Monad
import Control.Applicative

fact :: Integer -> Integer
fact 0 = 1
fact n = foldl' (*) 1 [1..n]

choose :: Integer -> Integer -> Integer
choose n k | k < 0     = 0
           | k > n     = 0
           | otherwise = fact n `div` (fact k * fact (n-k))

main :: IO() 
main = do
    t <- readLn
    replicateM_ t $ do
        [n, k] <- map read . words <$> getLine
        print . (`mod` 100000007) $ n `choose` k
