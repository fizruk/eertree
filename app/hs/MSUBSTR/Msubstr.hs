-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)
import           EERTREE.Applications
import           EERTREE.Simple

processMsubstrLine :: Int -> IO()
processMsubstrLine 0 = return ()
processMsubstrLine x = do
    str <- getLine
    let (lenn, freq) = (getMsubstr str)
    putStrLn(show lenn ++ " " ++ show freq)
    processMsubstrLine (x - 1)

main :: IO ()
main = do
  t <- readLn @Int
  (processMsubstrLine t)
