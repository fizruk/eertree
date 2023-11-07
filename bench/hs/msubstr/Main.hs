{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           GHC.TypeLits    (KnownNat, natVal)
import           Control.Monad   (replicateM)

import           EERTREE.Applications
import           Criterion.Main
import           Test.QuickCheck

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

main :: IO ()
main = defaultMain
  [ bgroup "getMsubstr"
    [ benchmarkGetMsubstr 500, benchmarkGetMsubstr 1000,
      benchmarkGetMsubstr 2000, benchmarkGetMsubstr 3000
    ]
  ]
