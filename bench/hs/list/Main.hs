{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Data.Proxy      (Proxy (..))
import           GHC.TypeLits    (KnownNat, natVal)

import           EERTREE.List
import           EERTREE.Random  hiding (randomEERTREE, randomEERTREEpairIO)
import           EERTREE.Symbol

import           Criterion.Main
import           Test.QuickCheck

-- | Generator for eertree from random list of symbols of length @len@
randomEERTREE :: KnownNat n => Int -> Gen (EERTREE n)
randomEERTREE len = do
  symbols <- randomSymbols len
  let t = eertree symbols
  return t

-- | Generate pair of random eertrees of lengths @len1@ and @len2@
randomEERTREEpairIO :: KnownNat n => Int -> Int -> IO (EERTREE n, EERTREE n)
randomEERTREEpairIO len1 len2 = do
    t1 <- generate (randomEERTREE len1)
    t2 <- generate (randomEERTREE len2)
    return (t1, t2)

-- | Build eertree from a given list of symbols
-- and show its @maxSuffix@
benchRandomEERTREE :: KnownNat n => [Symbol n] -> String
benchRandomEERTREE s = show (maxSuffix (eertree s))

-- | For a given alphabet size and a list of eertree lengths
-- construct a list of corresponding eertree benchmarks
eertreeBenchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
eertreeBenchmarkList _ lens = [ env (randomSymbolsIO @n len)
                                (\s ->
                                  bgroup ("eertree @" ++ show (natVal (Proxy @n)))
                                  [ bench ("len " ++ show len) $ nf benchRandomEERTREE s ]
                                )
                              | len <- lens ]

-- | Merge two eertrees and show the number of new palindromes
benchRandomMerge :: KnownNat n => EERTREE n -> EERTREE n -> Bool
benchRandomMerge t1 t2 = maxPrefix t == maxPrefix t1 &&
                         maxSuffix t == maxSuffix t2
  where
    t = merge t1 t2

-- | For a given alphabet size and a list of eertree lengths
-- construct a list of corresponding merge benchmarks
mergeBenchmarkList :: forall n. KnownNat n => Proxy n -> [(Int, Int)] -> [Benchmark]
mergeBenchmarkList _ lens = [ env (randomEERTREEpairIO @n len1 len2)
                              (\ ~(t1, t2) ->
                                bgroup ("merge @" ++ show (natVal (Proxy @n)))
                                [ bench ("len " ++ show len1 ++ ":" ++ show len2) $ nf (benchRandomMerge t1) t2 ]
                              )
                            | (len1, len2) <- lens ]

main :: IO ()
main = defaultMain
  ( eertreeAt2 ++ eertreeAt4 ++
    mergeLeftAt2 ++ mergeRightAt2 ++ mergeAt2 ++
    mergeLeftAt4 ++ mergeRightAt4 ++ mergeAt4
  )
    where
      s = 10000

      -- | Lists of benchmarks for eertrees of lenths 1s, 2s, 4s, 8s, and 16s
      -- and alphabet sizes 2 and 4 respectively
      eertreeAt2 = eertreeBenchmarkList (Proxy @2) powersOf2
      eertreeAt4 = eertreeBenchmarkList (Proxy @4) powersOf2

      -- | List of 1s, 2s, 4s, 8s, and 16s
      powersOf2 = take 5 [ s * 2^x | x <- [0..] ]

      -- | Lists of benchmarks for eertree merges of varying lengths
      -- and alphabet sizes 2 and 4 respectively.
      --
      -- "mergeLeft" means the left tree would be prepended to the right one.
      -- "mergeLeft" means the right tree would be appended to the left one.
      -- "merge" means that the trees have the same sizes, and is the same as "mergeLeft".
      mergeLeftAt2  = mergeBenchmarkList (Proxy @2) rightPowersOf2
      mergeRightAt2 = mergeBenchmarkList (Proxy @2) leftPowersOf2
      mergeAt2      = mergeBenchmarkList (Proxy @2) bothPowersOf2
      mergeLeftAt4  = mergeBenchmarkList (Proxy @4) rightPowersOf2
      mergeRightAt4 = mergeBenchmarkList (Proxy @4) leftPowersOf2
      mergeAt4      = mergeBenchmarkList (Proxy @4) bothPowersOf2

      -- | List of pairs (1s, 1s), (2s, 1s), (4s, 1s), (8s, 1s), (16s, 1s)
      leftPowersOf2 = take 5 [ (s * 2^x, s) | x <- [0..] ]

      -- | List of pairs (1s, 1s), (1s, 2s), (1s, 4s), (1s, 8s), (1s, 16s)
      rightPowersOf2 = take 5 [ (s, s * 2^x) | x <- [0..] ]

      -- | List of pairs (1s, 1s), (2s, 2s), (4s, 4s), (8s, 8s), (16s, 16s)
      bothPowersOf2 = take 5 [ (s * 2^x, s * 2^x) | x <- [0..] ]
