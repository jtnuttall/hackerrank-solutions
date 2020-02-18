import Control.Applicative
import Control.Monad
import Control.Monad.State

merged :: Ord a => [[a]] -> State Int [a]
merged [a] = return a
merged xs  = do
    xs' <- mergesort' xs
    merged xs'

mergesort' :: Ord a => [[a]] -> State Int [[a]]
mergesort' [a]      = return [a]
mergesort' [a,b]    = do
    xs <- merge a b
    return [xs]
mergesort' (a:b:xs) = do
    x  <- merge a b
    xs <- mergesort' xs
    return (x : xs)

merge :: Ord a => [a] -> [a] -> State Int [a]
merge xs [] = return xs
merge [] ys = return ys
merge (x:xs) (y:ys) 
    | (x <= y)  = do
        xs' <- merge xs (y:ys)
        return (x : xs')
    | otherwise = do
        modify' (+ 1)
        ys' <- merge (x:xs) ys
        return (y : ys')

mergesort :: Ord a => [a] -> State Int [a]
mergesort xs = do
    let xs' = map return xs
    result <- mergesort' xs'
    merged result
    
solve :: [Int] -> ([Int], Int)
solve = flip runState 0 . mergesort

main :: IO ()
main = do
    t <- readLn
    replicateM_ t $ do
        _ <- getLine
        xs <- map read . words <$> getLine
        
        let (sorted, flips) = solve xs
        print flips

