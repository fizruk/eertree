{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           GHC.TypeLits    (KnownNat, natVal)
import           Control.Monad   (replicateM)

import           EERTREE.ApplicationsDE
import           Criterion.Main
import           Test.QuickCheck

-- | Generate a random lowercase English letter
-- or '-' for deleting the last inserted character
getRandomLetterWithDelete :: Gen Char
getRandomLetterWithDelete = elements (['a'..'z'] ++ ['-'])

-- | Generate a random lowercase English letter
getRandomLetterNoDelete :: Gen Char
getRandomLetterNoDelete = elements ['a'..'z']

-- | Generate random queries of inserting characters or deleting the last inserted
genRandomQueries :: Int -> Int -> Gen String
genRandomQueries 0 _ = return ""
genRandomQueries len 0 = do
  char <- getRandomLetterNoDelete
  rest <- genRandomQueries (len - 1) 1
  return (char : rest)
genRandomQueries len cnt = do
  char <- if cnt > 0 then getRandomLetterWithDelete else getRandomLetterNoDelete
  rest <- case char of
            '-' ->  genRandomQueries (len - 1) (cnt - 1)
            _   -> genRandomQueries (len - 1) (cnt + 1)
  return (char : rest)

benchmarkGetQueries :: Int -> Benchmark
benchmarkGetQueries len = bench ("length " ++ show len) $ nfIO $ do
  randomString <- generate (genRandomQueries len 0)
  return $ getQueries randomString

main :: IO ()
main = defaultMain
  [ bgroup "Queries"
    [ benchmarkGetQueries (n * 2000) | n <- [1..11] ]
  ]
