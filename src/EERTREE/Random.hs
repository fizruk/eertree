{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Random where

import           Control.Monad   (replicateM)
import           Data.Proxy
import           EERTREE.Simple
import           EERTREE.Symbol  (alphabet)
import           GHC.TypeLits    (KnownNat)
import           Test.QuickCheck

genEERTREE :: KnownNat n => Int -> Gen (EERTREE n)
genEERTREE len = eertree <$> replicateM len (elements alphabet)

genMerge :: forall n. KnownNat n => Proxy n -> Int -> Gen Int
genMerge _ len = do
  e1 <- genEERTREE @n len
  e2 <- genEERTREE @n len
  let e = merge e1 e2
      lp = length . palindromes
      new = lp e - (lp e1 + lp e2)
  return new

-- |
-- >>> generate (genAverageMerge (Proxy @2) 100 500)
-- 7.416
-- >>> generate (genAverageMerge (Proxy @4) 100 500)
-- 3.09
-- >>> generate (genAverageMerge (Proxy @33) 100 500)
-- 2.024
genAverageMerge :: forall n. KnownNat n => Proxy n -> Int -> Int -> Gen Double
genAverageMerge n len sampleSize = do
  xs <- replicateM sampleSize (genMerge n len)
  return (fromIntegral (sum xs) / fromIntegral sampleSize)
