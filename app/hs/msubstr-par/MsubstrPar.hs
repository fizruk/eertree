-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Data.List (foldl')
import EERTREE.Applications.Unidirectional (getMsubstrPar)
import           EERTREE.Node
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Control.Parallel.Strategies

processMsubstrLine :: Int -> IO()
processMsubstrLine 0 = return ()
processMsubstrLine x = do
    str <- Text.getLine
    let !(lenn, freq) = getMsubstrPar str
    putStrLn(show lenn ++ " " ++ show freq)
    processMsubstrLine (x - 1)

main :: IO ()
main = do
  t <- readLn @Int
  processMsubstrLine t
