{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Random where

import           Control.Monad          (replicateM)
import           Data.Proxy             (Proxy)
import           GHC.TypeLits           (KnownNat)

import           EERTREE.Alphabet.Class (alphabet)
import           EERTREE.Simple
import           EERTREE.Symbol

import           Test.QuickCheck

-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XOverloadedStrings

-- | Generate list of random symbols of length @len@
--
-- > genRandomSymbols @4 10
-- [3,2,3,1,2,3,0,0,2,0]
randomSymbolsIO :: KnownNat n => Int -> IO [Symbol n]
randomSymbolsIO n = generate (randomSymbols n)

-- | Generator for list of random symbols of length @len@
randomSymbols :: KnownNat n => Int -> Gen [Symbol n]
randomSymbols len = replicateM len (elements alphabet)

-- | Generator for eertree from random list of symbols of length @len@
randomEERTREE :: KnownNat n => Int -> Gen (EERTREE (Symbol n))
randomEERTREE len = do
  symbols <- randomSymbols len
  let t = eertree symbols
  return t

-- | Generate pair of random eertrees of lengths @len1@ and @len2@
randomEERTREEpairIO :: KnownNat n => Int -> Int -> IO (EERTREE (Symbol n), EERTREE (Symbol n))
randomEERTREEpairIO len1 len2 = do
    t1 <- generate (randomEERTREE len1)
    t2 <- generate (randomEERTREE len2)
    return (t1, t2)

-- | Count number of new palindromes after merging two random eertrees
randomMerge :: forall n. KnownNat n => Proxy n -> Int -> Gen Int
randomMerge _ len = do
  t1 <- randomEERTREE @n len
  t2 <- randomEERTREE @n len
  let t = merge t1 t2
      palLen = length . palindromes
      newPals = palLen t - (palLen t1 + palLen t2)
  return newPals

-- |
-- @
-- > generate (genAverageMerge (Proxy @2) 100 500)
-- 7.416
-- > generate (genAverageMerge (Proxy @4) 100 500)
-- 3.09
-- > generate (genAverageMerge (Proxy @33) 100 500)
-- 2.024
-- @
genAverageMerge :: forall n. KnownNat n => Proxy n -> Int -> Int -> Gen Double
genAverageMerge n len sampleSize = do
  xs <- replicateM sampleSize (randomMerge n len)
  return (fromIntegral (sum xs) / fromIntegral sampleSize)
