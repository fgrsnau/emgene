module PWM where

import Data.List
import Data.List.Split

import Numeric.Container

import System.Random

import Util

-- | Data type for nucleotide and nucleotide sequences.
data Nucleotide = A | C | G | T deriving (Bounded, Enum, Eq, Ord, Read, Show)

type Sequence = [Nucleotide]

-- | Accumulates a list of associations (tuple with index and value) to a matrix
-- by summing up elements with the same index.
assocsToMatrix :: Int -> [((Int, Int), Double)] -> Matrix Double
assocsToMatrix l = accum empty (+)
  where
    dimension = fromEnum (maxBound :: Nucleotide) + 1
    empty = (dimension >< l) (repeat 0)

-- | Returns the weighted frequency of characters in a single sequence as a list
-- of associations (tuple with index and value).
weightedFrequencyToAssocs :: Double -> Sequence -> [((Int, Int), Double)]
weightedFrequencyToAssocs w = zipWith f [0..]
  where
    f i c = ((fromEnum c, i), w)

-- | Returns the frequency of characters in a single sequence as a list of
-- associations (tuple with index and value).
frequencyToAssocs :: Sequence -> [((Int, Int), Double)]
frequencyToAssocs = weightedFrequencyToAssocs 1

-- | Calculates the frequency matrix for the weighted sequences.
weightedFrequency :: Int -> [(Double, Sequence)] -> Matrix Double
weightedFrequency l = assocsToMatrix l
                    . concatMap (uncurry weightedFrequencyToAssocs)

-- | Calculates the frequency matrix for all sequences.
frequency :: Int -> [Sequence] -> Matrix Double
frequency l = assocsToMatrix l . concatMap (weightedFrequencyToAssocs 1)

-- | Introduces pseudocounts. Should be used on the return value of `frequency`.
pseudocounts :: Vector Double -> Matrix Double -> Matrix Double
pseudocounts v = fromRows . zipWith addConstant (toList v) . toRows

-- | Transform a frequency matrix to a probability matrix by normalizing all the
-- columns.
probability :: Matrix Double -> Matrix Double
probability = normalizeColumns

-- | Calculates $M_rel$.
relativeLog :: Vector Double -> Matrix Double -> Matrix Double
relativeLog priors' = fromRows . zipWith f (toList priors') . toRows
  where
    f p v = ld (scale (recip p) v)

-- | Implements the (+) operator.
(<+>) :: Matrix Double -> Sequence -> Double
m <+> s = foldl1' (+) $ zipWith (@>) (toColumns m) (map fromEnum s)

-- | Calculates the background probability vector by counting all nucleotides.
backgroundProbabilities :: [Sequence] -> Vector Double
backgroundProbabilities = normalizeVector . accum (constant 0 4) (+)
                        . concatMap toAssoc
  where
    toAssoc = map $ \c -> (fromEnum c, 1)

-- | Calculates the initial M_rel for the given sequences and given starting
-- positions.
initialGuess :: Int -> Vector Double -> [(Int, Sequence)]-> Matrix Double
initialGuess k p = relativeLog p . probability . pseudocounts p
                 . frequency k . map (uncurry $ cut k)

-- | Generates an infinite stream of random start positions.
randomStartPositions :: RandomGen g => g -> Int -> Int -> Int -> [[Int]]
randomStartPositions g n l k = chunksOf n $ randomRs (1, maximumIndex) g
  where
    maximumIndex = l - k + 1

-- vim: set ts=4 sts=4 sw=4 et:
