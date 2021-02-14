{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Criterion.Main

import           EERTREE.Simple
import           EERTREE.Symbol

-- | String of ones
benchMaxSuffixOne :: Int -> String
benchMaxSuffixOne n = show (maxSuffix (eertree @2 (replicate n 1)))

-- | Eertree from string of ones
benchMaxSuffixOneFromString :: Int -> String
benchMaxSuffixOneFromString n = show (maxSuffix (eertreeFromString @2 (take n (cycle "1"))))

-- | Alphabet
benchMaxSuffixAlpha :: Int -> String
benchMaxSuffixAlpha n = show (maxSuffix (eertree @5000 [0 .. Symbol (n - 1)]))

-- | 20% of prepends go through case one
benchMaxSuffix20 :: Int -> String
benchMaxSuffix20 n = show (maxSuffix (eertree @2 (take n (cycle [1,0,1,0,0]))))

-- | 50% of prepends go through case one
benchMaxSuffix50 :: Int -> String
benchMaxSuffix50 n = show (maxSuffix (eertree @2 (take n (cycle [1,0]))))


main :: IO ()
main = defaultMain
  [ bgroup "maxSuffix . eertree (1)"
    [ bench  "500" $ nf benchMaxSuffixOne  500
    , bench "1000" $ nf benchMaxSuffixOne 1000
    , bench "2000" $ nf benchMaxSuffixOne 2000
    , bench "4000" $ nf benchMaxSuffixOne 4000 ]
  
  , bgroup "maxSuffix . eertreeFromString '1'"
    [ bench  "500" $ nf benchMaxSuffixOneFromString  500
    , bench "1000" $ nf benchMaxSuffixOneFromString 1000
    , bench "2000" $ nf benchMaxSuffixOneFromString 2000
    , bench "4000" $ nf benchMaxSuffixOneFromString 4000 ]
  
  , bgroup "maxSuffix . eertree (Alpha)"
    [ bench  "500" $ nf benchMaxSuffixAlpha  500
    , bench "1000" $ nf benchMaxSuffixAlpha 1000
    , bench "2000" $ nf benchMaxSuffixAlpha 2000
    , bench "4000" $ nf benchMaxSuffixAlpha 4000 ]
  
  , bgroup "maxSuffix . eertree (20%)"
    [ bench  "500" $ nf benchMaxSuffix50  500
    , bench "1000" $ nf benchMaxSuffix50 1000
    , bench "2000" $ nf benchMaxSuffix50 2000
    , bench "4000" $ nf benchMaxSuffix50 4000 ]

  , bgroup "maxSuffix . eertree (50%)"
  [ bench  "500" $ nf benchMaxSuffix50  500
  , bench "1000" $ nf benchMaxSuffix50 1000
  , bench "2000" $ nf benchMaxSuffix50 2000
  , bench "4000" $ nf benchMaxSuffix50 4000 ]
  ]
