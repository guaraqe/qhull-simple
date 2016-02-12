module Main where

import Criterion.Main
import Control.DeepSeq
import Control.Exception(evaluate)
import Data.Either
import Control.Concurrent(threadDelay)

import qualified Numeric.Qhull as QH
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V

readPoints :: String -> V.Vector (UV.Vector Double)
readPoints s = V.fromList $ map (UV.fromList . (map read) . words) (lines s)

test :: Int -> UV.Vector Double -> [SV.Vector Int]
test d pts = either id (error "INVALID BENCHMARK") (QH.qhull d pts)

test' :: V.Vector (UV.Vector Double) -> [SV.Vector Int]
test' pts = either id (error "INVALID BENCHMARK") (QH.qhull' pts)

sleep :: Int -> IO ()
sleep s = threadDelay (10^6 * s)

main :: IO ()
main = getContents >>= \input ->
       let
           points = readPoints input
           packedPoints = UV.concat (V.toList points)
           d = UV.length (V.head points)
       in
       evaluate (force points) >>= \points' ->
       evaluate (force packedPoints) >>= \packedPoints' ->
       putStrLn ("Read " ++ show (V.length points') ++ " points.") >>
       putStrLn ("Sleeping to let the system settle down.") >>
       sleep 2 >>
       defaultMain [
        bgroup "Benchmark on stdin" [
         bench "qhull"  $ nf (test d) packedPoints', 
         bench "qhull'"  $ nf test' points'
        ]
       ]