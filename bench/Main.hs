{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Criterion.Main

import           EERTREE.Simple

benchMaxSuffix :: Int -> String
benchMaxSuffix n = show (maxSuffix (eertreeFromString @2 (take n (cycle "10100"))))

main :: IO ()
main = defaultMain
  [ bgroup "maxSuffix . eertreeFromString"
    [ bench  "500" $ nf benchMaxSuffix  500
    , bench "1000" $ nf benchMaxSuffix 1000
    , bench "1500" $ nf benchMaxSuffix 1500
    , bench "2000" $ nf benchMaxSuffix 2000
    , bench "2500" $ nf benchMaxSuffix 2500
    , bench "3000" $ nf benchMaxSuffix 3000
    ]
  ]
