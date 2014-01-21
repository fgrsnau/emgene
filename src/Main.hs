module Main where

import Control.Parallel.Strategies

import Data.List
import Data.Ord

import Numeric.Container hiding ((<>))

import Options.Applicative

import System.IO
import System.Random

import Alternative
import EM
import EMLike
import Formatting
import Parser
import PWM
import Util

-- | The structure for storing the command line options.
data Settings = Settings
    { filename    :: FilePath
    , rounds      :: Int
    , convergence :: Double
    , alternative :: Bool
    }

-- | The command line parser.
cmdline :: Parser Settings
cmdline = Settings
    <$> strOption ( short 'f' <> long "filename" <> metavar "INPUT"
                 <> help "Input filename." )
    <*> option ( short 'r' <> long "rounds" <> metavar "COUNT"
              <> help "Number of random rounds." <> value 100  <> showDefault )
    <*> option ( short 'c' <> long "convergence" <> metavar "DOUBLE"
              <> help "Convergence epsilon." <> value 1e-3 <> showDefault )
    <*> switch ( short 'a' <> long "altenative"
              <> help "Whether to use alternative algorithm." )

-- | Processes a single random try.
processRandomTry :: Double -> EMLike a -> Int -> Vector Double -> [Sequence] -> [Int] -> [Sequence]
processRandomTry eps e k p seqs idxs = zipWith (cut k) (mrelToPositions e mrel) seqs
  where
    mrel  = em eps e guess
    guess = initialGuess k p $ zip idxs seqs

-- | Process the final EM iteration, which is initialized with the most matching
-- motif.
processFinal :: Double -> EMLike a -> Int -> Vector Double -> Sequence -> Matrix Double
processFinal eps e k p s
    = em eps e . relativeLog p . probability . pseudocounts p $ frequency k [s]

-- | Processes all the EM iterations. At first there are several EM iterations
-- with random start positions. From this runs the most matching motif is
-- selected and a final EM iteration uses this motif as initialisation.
processAll :: Double -> EMLike a -> Int -> Vector Double -> [Sequence] -> [[Int]] -> IO ()
processAll eps e k p seqs idxss = do
    let motifs = concat $ parMap rseq (processRandomTry eps e k p seqs) idxss
        best   = head . maximumBy (comparing length) . group $ sort motifs
    hPutStr stderr "Found best motif: " >> hPrint stderr best

    let mrel = processFinal eps e k p best
        idxs = mrelToPositions e mrel
    hPutStr stderr "M_rel = " >> hPutStr stderr (dispf 3 mrel)
    hPutStr stderr "Found indices: " >> hPrint stderr idxs

    mapM_ putStrLn . formatSequences k $ zip idxs seqs

-- | The main routine of the program.
main :: IO ()
main = do
    (Settings f r c a) <- execParser $ info (helper <*> cmdline) fullDesc
    parsed <- parseInputFile f
    case parsed of
        Left err -> do
            hPutStrLn stderr "Error parsing input file:"
            hPutStrLn stderr err
        Right (Configuration n l k seqs) -> do
            g <- getStdGen
            putStr  "N=" >> putStr   (show n)
            putStr " K=" >> putStr   (show k)
            putStr " L=" >> putStrLn (show l)
            let starts = take r $ randomStartPositions g n l k
                backs  = backgroundProbabilities seqs
                run e  = processAll c e k backs seqs starts

            if a then run (alternativeEM k backs seqs)
                 else run (originalEM k backs seqs)

-- vim: set ts=4 sts=4 sw=4 et:
