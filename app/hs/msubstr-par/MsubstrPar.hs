-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Data.List (foldl')
import EERTREE.List ( EERTREE(palindromes), eertreeFromString, merge, empty)
import           EERTREE.Node
import GHC.Conc (numCapabilities)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Control.Parallel.Strategies

countMax :: Ord a => a -> [a] -> (a, Int)
countMax z = foldl' step (z, 1)
  where
    step acc@(x, n) y
      | y > x = (y, 1)
      | y == x = (x, n + 1)
      | otherwise = acc

countMax1 :: Ord a => [a] -> (a, Int)
countMax1 [] = error "empty list"
countMax1 (x:xs) = countMax x xs

getMsubstrPar :: Int -> Text -> (Int, Int)
getMsubstrPar cores input = countMax1 (map len (palindromes tree))
  where
    chunkSize = Text.length input `div` fromIntegral cores
    tree = foldr merge empty chunks
    chunks =
      [ eertreeFromString @26 (Text.unpack chunk)
      | chunk <- Text.chunksOf chunkSize input
      ] `using` parList rpar

processMsubstrLine :: Int -> IO()
processMsubstrLine 0 = return ()
processMsubstrLine x = do
    str <- Text.getLine
    let !(lenn, freq) = getMsubstrPar numCapabilities str
    putStrLn(show lenn ++ " " ++ show freq)
    processMsubstrLine (x - 1)

main :: IO ()
main = do
  t <- readLn @Int
  processMsubstrLine t
