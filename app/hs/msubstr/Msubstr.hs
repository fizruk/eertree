-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Data.Foldable as F
import EERTREE.Applications.Unidirectional (getMsubstr)
import           EERTREE.Node

processMsubstrLine :: Int -> IO()
processMsubstrLine 0 = return ()
processMsubstrLine x = do
    str <- getLine
    let !(lenn, freq) = getMsubstr str
    putStrLn(show lenn ++ " " ++ show freq)
    processMsubstrLine (x - 1)

main :: IO ()
main = do
  t <- readLn @Int
  processMsubstrLine t
