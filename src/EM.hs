module EM (originalEM) where

import Numeric.Container

import PWM
import Util
import qualified EMLike as L

-- | The 'L.EMLike' structure for the original EM algorithm.
originalEM :: Int -> Vector Double -> [Sequence] -> L.EMLike (Matrix Double)
originalEM k p seqs = L.EMLike
    { L.expectation  = expectation k seqs
    , L.maximization = maximization k p seqs
    , L.positions    = maxCols
    }

-- | Computes Pr(z^j = i | S^j, M_rel) for all j, i.
expectation :: Int -> [Sequence] -> Matrix Double -> Matrix Double
expectation k seqs mrel = normalizeRows . fromRows $ map seqToRowVec seqs
  where
    seqToRowVec :: Sequence -> Vector Double
    seqToRowVec = fromList . map ((2 **) . (mrel <+>)) . cuts k

-- | Computes M^{t+1} = argmax_M Q(M | M^t).
maximization :: Int -> Vector Double -> [Sequence] -> Matrix Double -> Matrix Double
maximization k p seqs
    = relativeLog p . probability . pseudocounts p . assocsToMatrix k . concat
    . zipWith processSeq seqs . toRows 
  where
    processSeq :: Sequence -> Vector Double -> [((Int, Int), Double)]
    processSeq s w = concat $ zipWith weightedFrequencyToAssocs (toList w) (cuts k s)

-- vim: set ts=4 sts=4 sw=4 tw=80 et:
