------------------------------------------------------------------------------------------
-- Haskell implementation of the Knuth-Morris-Pratt substring searching algorithm
--
-- https://www.hackerrank.com/challenges/kmp-fp
------------------------------------------------------------------------------------------
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Array.ST
import Data.Array.Unboxed

(-!) :: C.ByteString -> Int -> Char
(-!) = C.index

mkTable :: C.ByteString -> UArray Int Int
mkTable pat = runSTUArray $ do
    tbl <- newListArray (0, C.length pat - 1) $ (-1) : replicate (C.length pat - 1) 0
    writeArray tbl 0 (-1)
    let make p c 
            | p >= C.length pat = return tbl
            | otherwise         = proc
            where 
                proc | pat -! (p-1) == pat -! c
                                 = writeArray tbl p (c+1) >> make (p+1) (c+1)
                     | c > 0     = readArray tbl c >>= make p
                     | otherwise = writeArray tbl p 0 >> make (p+1) c
    make 2 0

kmp :: C.ByteString -> C.ByteString -> UArray Int Int -> String
kmp text pat tbl = search 0 0
    where
        l = C.length text
        search m i
            | m + i >= l = "NO"
            | otherwise  = cond
            where
                cond | pat -! i == text -! (m+i)
                          = if i == C.length pat - 1
                            then "YES"
                            else search m (i+1)
                     | tbl ! i > (-1)
                          = search (m + i - (tbl ! i)) (tbl ! i)
                     | otherwise
                          = search (m+1) 0

main :: IO()
main = do
    t <- readLn
    replicateM_ t $ do
        text <- C.getLine
        pat  <- C.getLine
        putStrLn . kmp text pat $ mkTable pat