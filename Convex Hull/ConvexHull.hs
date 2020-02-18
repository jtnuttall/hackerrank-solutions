module ConvexHull where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Ord
import qualified Data.Set as Set
import Text.Printf

type Point = (Double, Double)

type Hull = Set.Set Point

toPoint :: [Double] -> Point
toPoint (x:y:_) = (x,y)

lineDist' :: Point -> Point -> Point -> Double
lineDist' (x1, y1) (x2, y2) (x, y) = 
    (y - y1) * (x2 - x1) - (y2 - y1) * (x - x1)

lineDist :: Point -> Point -> Point -> Double
lineDist = ((abs .) .) . lineDist'

lineSide :: Point -> Point -> Point -> Int
lineSide = (((round . signum) .) .) . lineDist'

maxX :: [Point] -> Point
maxX = maximumBy (comparing fst)

minX :: [Point] -> Point
minX = minimumBy (comparing fst)

maxY :: [Point] -> Point
maxY = maximumBy (comparing snd)

minY :: [Point] -> Point
minY = minimumBy (comparing snd)

findHull :: [Point] -> Point -> Point -> State Hull ()
findHull points p1 p2 =
    unless (null points) $ do
        let c = maximumBy (comparing $ lineDist p1 p2) points
            s1 = filter ((>0) . lineSide p1 c) points
            s2 = filter ((>0) . lineSide c p2) points

        modify $ Set.insert c

        findHull s1 p1 c
        findHull s2 c p2

quickHull :: [Point] -> State Hull ()
quickHull points = do
    let minPoint = minX points
        maxPoint = maxX points
        s1 = filter ((>0) . lineSide minPoint maxPoint) points
        s2 = filter ((>0) . lineSide maxPoint minPoint) points

    modify $ Set.insert minPoint
    modify $ Set.insert maxPoint

    findHull s1 minPoint maxPoint
    findHull s2 maxPoint minPoint

perimeter :: [Point] -> Double
perimeter []  = 0.0
perimeter [_] = 0.0
perimeter ((x1,y1):(x2,y2):ps) =
    let f1 = x1 - x2
        f2 = y1 - y2
    in sqrt (f1*f1 + f2*f2) + perimeter ((x2,y2):ps)

solve :: [Point] -> Double
solve points =
    let hull = Set.toList $ execState (quickHull points) Set.empty
        cx = (fst (maxX hull) - fst (minX hull)) / 2.0
        cy = (snd (maxY hull) - snd (minY hull)) / 2.0
        angle (x,y) = atan2 (y - cy) (x - cx)
        angles = map (\p -> (angle p, p)) hull
        hull'@(x:_) = map snd . sortBy (comparing fst) $ angles
    in perimeter (hull' ++ [x])

main :: IO ()
main = do
    n <- readLn
    points <- map (toPoint . map read . words) <$> replicateM n getLine
    printf "%.1f\n" $ solve points

