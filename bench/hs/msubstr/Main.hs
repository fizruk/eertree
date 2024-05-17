{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           GHC.TypeLits    (KnownNat, natVal)
import           Control.Monad   (replicateM)

import           EERTREE.Applications.Unidirectional (getMsubstr)
import           EERTREE.Applications.Bidirectional (getMsubstrPar)

import           Criterion.Main
import           Test.QuickCheck
import Data.Text.Lazy (Text, pack)

-- Generate a random lowercase English letter
genRandomLetter :: Gen Char
genRandomLetter = elements ['a'..'z']

-- Generate a random string of lowercase English letters of a specified length
genRandomString :: Int -> Gen String
genRandomString len = replicateM len genRandomLetter

benchmarkGetMsubstr :: Int -> Benchmark
benchmarkGetMsubstr len = bench ("length " ++ show len) $ nfIO $ do
  randomString <- generate (genRandomString len)
  return $ getMsubstr randomString

benchmarkGetMsubstrPar :: Int -> Benchmark
benchmarkGetMsubstrPar len = bench ("length " ++ show len) $ nfIO $ do
  randomString <- generate (genRandomString len)
  return $ getMsubstrPar (pack randomString)

main :: IO ()
main = defaultMain
  [ bgroup "Msubstr"
    [ benchmarkGetMsubstr (n * 2000) | n <- [1..11] ],
    bgroup "Msubstr Parallel"
    [ benchmarkGetMsubstrPar (n * 2000) | n <- [1..11] ]
  ]
