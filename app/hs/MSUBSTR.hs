-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)
import           EERTREE.Simple
import           EERTREE.Node
import           GHC.TypeLits                (KnownNat)
import Data.Char

maxSubLengthSum :: [(Node n, Int)] -> (Int, Int)
maxSubLengthSum listFreq =
  let
    maxLength = maximumBy (comparing (len . fst)) listFreq
    -- Get the nodes that have the maximum length only
    maxNodes = filter (\(node, _) -> len node == len (fst maxLength)) listFreq
    freqValues = map snd maxNodes
  in
    (len (fst maxLength), (sum freqValues))

getLongestSubpalindromeLength :: KnownNat n => (EERTREE n) -> (Int, Int)
getLongestSubpalindromeLength tree = maxSubLengthSum (frequency tree)

processLine :: Int -> IO()
processLine 0 = return ()
processLine x = do
    str <- getLine
    let tree = eertreeFromString @26 str
    let (lenn, freq) = (getLongestSubpalindromeLength tree)
    putStrLn( show lenn ++ " " ++ show freq)
    processLine (x - 1)

main :: IO ()
main = do
  t <- readLn @Int
  (processLine t)