module Main where

import Control.Monad
import qualified Data.Set as Set

crack :: Set.Set String -> [Int] -> String -> Maybe [String]
crack _ _ [] = Just []
crack set lengths attempt =
  let candidates = map (`splitAt` attempt) lengths
      test [] = Nothing
      test ((c, attempt'):cs)
        | Set.member c set = liftM2 (:) (Just c) $ crack set lengths attempt'
        | otherwise = test cs
   in test candidates

solve :: [String] -> String -> Maybe [String]
solve passwords attempt =
  let set = Set.fromList passwords
      lengths = map length passwords
   in crack set lengths attempt

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    passwords <- words <$> getLine
    attempt <- getLine
    case solve passwords attempt of
      Just result -> do
        mapM_ (putStr . (++ " ")) result
        putStrLn ""
      Nothing -> putStrLn "WRONG PASSWORD"
