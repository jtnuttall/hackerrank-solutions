{-# OPTIONS_GHC -O2 #-}
import Control.Monad
import Control.Applicative
import Data.Char

cipher :: Int -> String -> String
cipher n = map (chr . convert)
    where 
        convert c
            | not $ isAlpha c = ord c
            | isUpper c       = wrap (ord c + n) 90
            | otherwise       = wrap (ord c + n) 122
            where wrap c' t = if c' > t then wrap (c' - 26) t else c'

main :: IO ()
main = do
    _ <- getLine
    s <- getLine
    k <- readLn
    putStrLn $ cipher k s
