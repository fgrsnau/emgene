module Formatting (formatSequence, formatSequences) where

import Data.Char

import PWM

generatePredicate :: Int -> Int -> [Bool]
generatePredicate k i = map f $ map (\x -> i - x) [1..]
  where
    f d
      | d > 0     = False
      | d > -k    = True
      | otherwise = False

formatWithPredicate :: Sequence -> [Bool] -> String
formatWithPredicate s = concat . zipWith f s
  where
    f n p
      | p         = show n
      | otherwise = map toLower $ show n

-- | Formats a single sequence. All k letters starting at index i of sequence
-- s will be printed capitalized.
formatSequence :: Int -> Int -> Sequence -> String
formatSequence k i s = formatWithPredicate s $ generatePredicate k i

-- | Formats a bunch of sequence. See 'formatSequence'.
formatSequences :: Int -> [(Int, Sequence)] -> [String]
formatSequences k = map (uncurry $ formatSequence k)

-- vim: set ts=4 sts=4 sw=4 et:
