{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Data.Proxy      (Proxy (..))
import           GHC.TypeLits    (KnownNat, natVal)

import           EERTREE.Random
import           EERTREE.Simple
import           EERTREE.Symbol

import           Criterion.Main
import           Test.QuickCheck

-- | Build eertree from a given list of symbols
-- and show its @maxSuffix@
benchRandomEERTREE :: KnownNat n => [Symbol n] -> String
benchRandomEERTREE s = show (maxSuffix (eertree s))

-- | For a given alphabet size and a list of eertree lengths
-- construct a list of corresponding benchmarks
benchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
benchmarkList _ lens = [ env (genRandomSymbols @n len)
                         (\s ->
                           bgroup ("eertree @" ++ show (natVal (Proxy @n)))
                           [ bench ("len " ++ show len) $ nf benchRandomEERTREE s ]
                         )
                       | len <- lens ]

main :: IO ()
main = defaultMain
  (listAt2 ++ listAt4)
    where
      -- | Lists of benchmarks for eertrees of lenths 1k, 2k, 4k, 8k, and 16k
      -- and for alphabet sizes 2 and 4 respectively
      listAt2 = benchmarkList (Proxy @2) (take 5 [ 1000 * 2^x | x <- [0..] ])
      listAt4 = benchmarkList (Proxy @4) (take 5 [ 1000 * 2^x | x <- [0..] ])
