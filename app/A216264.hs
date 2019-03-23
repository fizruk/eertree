{-# OPTIONS_GHC -Wall #-}
module Main where

import           Data.List      (intercalate)
import           EERTREE.Simple
import           System.IO      (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "How many elements of A216264 to compute?"
  putStr "n = "
  hFlush stdout
  n <- read <$> getLine
  putStrLn (intercalate ", " (map show (a216264 n)))
