module Util where

import Data.List
import Data.Ord

import Numeric.Container
import Numeric.LinearAlgebra ()

-- | Short function for logarithmus dualis
ld :: Floating a => a -> a
ld = logBase 2

-- | Returns a subsequnce of length k starting at index i.
cut :: Int -> Int -> [a] -> [a]
cut k i = take k . drop (i - 1)

-- | Returns all subsequences of length k.
cuts :: Int -> [a] -> [[a]]
cuts k s = map (take k) . zipWith const (iterate tail s) $ drop (k-1) s

-- | Checks two consecutive for convergence. If a convergence is found, the list
-- will be truncated.
convergenceOn :: (a -> a -> Double) -> Double -> [a] -> [a]
convergenceOn f c ls = map fst . takeWhile snd $ zip ls bs
  where
    bs = True : True : zipWith condition ls (tail ls)
    condition x y = f x y > c

-- | Returns the column indices for the biggest element in each row.
maxCols :: Matrix Double -> [Int]
maxCols = map row . toRows
  where
    row = fst . maximumBy (comparing snd) . zip [1 :: Int ..] . toList

-- | Normalizes the elements of a vector so that they sum up to 1.
normalizeVector :: Vector Double -> Vector Double
normalizeVector v = scale (recip $ sumElements v) v

-- | Normalizes the columns, so that each of column sums is 1.
normalizeColumns :: Matrix Double -> Matrix Double
normalizeColumns = fromColumns . map normalizeVector . toColumns

-- | Normalizes the row, so that each of row sums is 1.
normalizeRows :: Matrix Double -> Matrix Double
normalizeRows = fromRows . map normalizeVector . toRows

-- | Calculates the mean squared error for two matrices (elementwise + summing up).
meanSquaredError :: Matrix Double -> Matrix Double -> Double
meanSquaredError m m' = (/ n) . sumElements $ (m' - m)^^(2 :: Int)
  where
    n = fromIntegral $ rows m * cols m

-- vim: set ts=4 sts=4 sw=4 tw=80 et:
