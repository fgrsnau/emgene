module EMLike where

import Numeric.Container
import Util

-- | Represents a structure for all EM like algorithms. Each EM algorithm has
-- a expectation step, a maximization step and a function for determining the
-- final positions.
data EMLike a = EMLike
    { expectation  :: Matrix Double -> a
    , maximization :: a -> Matrix Double
    , positions    :: a -> [Int]
    }

-- | Converts the M_rel to the index positons.
mrelToPositions :: EMLike a -> Matrix Double -> [Int]
mrelToPositions e = positions e . expectation e

-- | Performs an EM step (combination of expectation step and maximization
-- step).
emStep :: EMLike a -> Matrix Double -> Matrix Double
emStep e = maximization e . expectation e

-- | Performs all the EM iterations until convergence. The result is the final
-- M_rel matrix.
em :: Double -> EMLike a -> Matrix Double -> Matrix Double
em eps e m = last . convergenceOn meanSquaredError eps $ iterate (emStep e) m

-- vim: set ts=4 sts=4 sw=4 et:
