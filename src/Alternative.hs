module Alternative (alternativeEM) where

import Numeric.Container

import PWM
import Util
import qualified EMLike as L

-- | The 'L.EMLike' structure for the alternative EM algorithm.
alternativeEM :: Int -> Vector Double -> [Sequence] -> L.EMLike [Int]
alternativeEM k p seqs = L.EMLike
    { L.expectation  = expectation k seqs
    , L.maximization = maximization k p seqs
    , L.positions    = id
    }

-- | Basically the same as the original one, but simplified.
expectation :: Int -> [Sequence] -> Matrix Double -> [Int]
expectation k seqs mrel = maxCols . fromRows $ map seqToRowVec seqs
  where
    seqToRowVec :: Sequence -> Vector Double
    seqToRowVec = fromList . map ((2 **) . (mrel <+>)) . cuts k

-- | Exactly the same as in the original EM.
maximization :: Int -> Vector Double -> [Sequence] -> [Int] -> Matrix Double
maximization k p seqs idxs = relativeLog p . probability . pseudocounts p
                           . frequency k $ zipWith (cut k) idxs seqs

-- vim: set ts=4 sts=4 sw=4 tw=80 et:
