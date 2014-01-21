module Parser (Configuration(..), parseInputFile) where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.Bifunctor

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Utils

import PWM

-- | The configuration describes the parsing result. Symbolic meaning:
-- @Configuration n l k seqs@.
data Configuration = Configuration Int Int Int [Sequence] -- n l k seqs

-- | Opens the given file and parses the content. Returns a configuration or
-- a String describing the error.
parseInputFile :: FilePath -> IO (Either String Configuration)
parseInputFile f = first show <$> parseFromFile parseConfiguration f

parseConfiguration :: Parser Configuration
parseConfiguration = do
    (n, l, k) <- parseHeader
    seqs <- count n $ parseSequence l
    void eof
    return $ Configuration n l k seqs

parseHeader :: Parser (Int, Int, Int)
parseHeader = do
    k <- string "K="  *> nat
    n <- string " N=" *> nat
    l <- string " L=" *> nat <* newline
    return (n, l, k)

parseSequence :: Int -> Parser Sequence
parseSequence l = count l parseNucleotide <* newline

parseNucleotide :: Parser Nucleotide
parseNucleotide = (read . pure . toUpper) <$> oneOf "acgtACGT"

-- vim: set ts=4 sts=4 sw=4 et:
