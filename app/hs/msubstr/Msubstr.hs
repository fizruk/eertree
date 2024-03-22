-- Get the length of the longest sub-palindrome
-- and the number of sub-palindromes with such length.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Data.Foldable as F
import EERTREE.List ( EERTREE(palindromes), eertreeFromString )
import           EERTREE.Node
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

getMsubstr :: String -> (Int, Int)
getMsubstr str = countMax1 (map len (palindromes tree))
  where
    tree = eertreeFromString @26 str

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
