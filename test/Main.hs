module Main where

import Numeric.Qhull
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Data.Either
import Data.List

readPoints :: String -> [UV.Vector Double]
readPoints s = map (UV.fromList . map read . words) (lines s)

simplexToString :: SV.Vector Int -> String
simplexToString x = unwords (map show (SV.toList x))

main' :: IO ()
main' =
  do input <- getContents
     let points = V.fromList (readPoints input)
         facets = either id (const []) (qhull' points)
         facets' = map (V.map (\i -> points V.! i) . SV.convert) facets
         output = unlines $ map simplexToString facets
     putStr output
     putStrLn $ either errorMessage show (delaunay'' testPoints2)


main = do
  putStrLn $ either errorMessage show (delaunay 2 test)

test = SV.fromList . concat $ testPoints

testPoints :: [[Double]]
testPoints = [ [0 , 0]
             , [1 , 0]
             , [0 , 1]
             , [1 , 0.5]
             , [-1,-1]]

testPoints2 :: [[Double]]
testPoints2 = [ [0,0,0]
              , [1,0,0]
              , [0,1,0]
              , [0,0,1]
              , [1,1,1]]


testCube :: [[Double]]
testCube = [[0,  0, 0],
            [1,  1, 1],
            [1, -1, 1],
            [-1, 1, 1],
            [1, 1, -1],
            [-1, -1, 1],
            [-1, 1, -1],
            [1, -1, -1],
            [-1,-1,-1]
           ]
