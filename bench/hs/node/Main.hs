{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Data.Proxy      (Proxy (..))
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import           GHC.TypeLits    (KnownNat, natVal)

import           EERTREE.Node
import           EERTREE.Random
import           EERTREE.Symbol

import           Criterion.Main
import           Test.QuickCheck

-- | Return value of a node
benchRandomNode :: KnownNat n => [Symbol n] -> Seq (Symbol n)
benchRandomNode s = value (fromPalindrome s)

-- | For a given alphabet size and a list of node lengths
-- construct a list of node benchmarks
nodeBenchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
nodeBenchmarkList _ lens = [ env (randomPalindromeIO @n len)
                                (\s ->
                                  bgroup ("node @" ++ show (natVal (Proxy @n)))
                                  [ bench ("len " ++ show len) $ nf benchRandomNode s ]
                                )
                           | len <- lens ]

-- | Benchmarks for edge and newSuffixOf with creating the environment

-- | Generate pair (symbol, node) to benchmark node functions
randomSymbolAndNodeIO :: KnownNat n => Int -> IO (Symbol n, Node n)
randomSymbolAndNodeIO len = do
  s    <- generate (elements alphabet)
  node <- generate (randomNode len)
  return (s, node)

-- | Return value of node created by @edge@
benchRandomNodeEdge :: KnownNat n => Symbol n -> Node n -> Seq (Symbol n)
benchRandomNodeEdge s node = value (edge s node)

-- | For a given alphabet size and a list of node lengths
-- construct a list of @edge@ benchmarks
edgeBenchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
edgeBenchmarkList _ lens = [ env (randomSymbolAndNodeIO @n len)
                                (\ ~(s, node) ->
                                  bgroup ("edge @" ++ show (natVal (Proxy @n)))
                                  [ bench ("len " ++ show len) $ nf (benchRandomNodeEdge s) node ]
                                )
                           | len <- lens ]

-- | Return value of node created by @newSuffixOf@
benchRandomNodeNewSuffixOf :: KnownNat n => Symbol n -> Node n -> Seq (Symbol n)
benchRandomNodeNewSuffixOf s node = value (newSuffixOf s node)

-- | For a given alphabet size and a list of node lengths
-- construct a list of @newSuffixOf@ benchmarks
newSuffixOfBenchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
newSuffixOfBenchmarkList _ lens = [ env (randomSymbolAndNodeIO @n len)
                                        (\ ~(s, node) ->
                                          bgroup ("newSuffixOf @" ++ show (natVal (Proxy @n)))
                                          [ bench ("len " ++ show len) $ nf (benchRandomNodeNewSuffixOf s) node ]
                                        )
                                  | len <- lens ]

-- | Benchmarks for edge and newSuffixOf without creating the environment
-- because fully computing nodes on depths 40+ takes a decent time

-- | Generate pair (symbol, symbols) to benchmark node functions
randomSymbolAndSumbolsIO :: KnownNat n => Int -> IO (Symbol n, [Symbol n])
randomSymbolAndSumbolsIO len = do
  c <- generate (elements alphabet)
  s <- generate (randomSymbols (len `div` 2))
  return (c, s ++ reverse s)

-- | Return value of node created by @edge@
benchRandomNodeEdge2 :: KnownNat n => Symbol n -> [Symbol n] -> Seq (Symbol n)
benchRandomNodeEdge2 c s = value (edge c (fromPalindrome s))

-- | For a given alphabet size and a list of node lengths
-- construct a list of @edge@ benchmarks
edgeBenchmarkList2 :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
edgeBenchmarkList2 _ lens = [ env (randomSymbolAndSumbolsIO @n len)
                                (\ ~(c, s) ->
                                  bgroup ("edge @" ++ show (natVal (Proxy @n)))
                                  [ bench ("len " ++ show len) $ nf (benchRandomNodeEdge2 c) s ]
                                )
                           | len <- lens ]

-- | Return value of node created by @newSuffixOf@
benchRandomNodeNewSuffixOf2 :: KnownNat n => Symbol n -> [Symbol n] -> Seq (Symbol n)
benchRandomNodeNewSuffixOf2 c s = value (newSuffixOf c (fromPalindrome s))

-- | For a given alphabet size and a list of node lengths
-- construct a list of @newSuffixOf@ benchmarks
newSuffixOfBenchmarkList2 :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
newSuffixOfBenchmarkList2 _ lens = [ env (randomSymbolAndSumbolsIO @n len)
                                        (\ ~(c, s) ->
                                          bgroup ("newSuffixOf @" ++ show (natVal (Proxy @n)))
                                          [ bench ("len " ++ show len) $ nf (benchRandomNodeNewSuffixOf2 c) s ]
                                        )
                                  | len <- lens ]

main :: IO ()
main = defaultMain
  ( nodeAt2 ++ nodeAt4 ++
    edgeAt2 ++ edgeAt4 ++
    newSuffixOfAt2 ++ newSuffixOfAt4
  )
    where
      s = 10

      -- | List of 1s, 2s, ..., 10s
      powersOf2 = take 10 [ s * x | x <- [1..] ]

      -- | Lists of benchmarks for node of lenths 1s, 2s, ..., 10s
      -- and alphabet sizes 2 and 4 respectively
      nodeAt2 = nodeBenchmarkList (Proxy @2) powersOf2
      nodeAt4 = nodeBenchmarkList (Proxy @4) powersOf2

      -- | Lists of benchmarks for edge of lenths 1s, 2s, ..., 10s
      -- and alphabet sizes 2 and 4 respectively
      edgeAt2 = edgeBenchmarkList2 (Proxy @2) powersOf2
      edgeAt4 = edgeBenchmarkList2 (Proxy @4) powersOf2

      -- | Lists of benchmarks for edge of lenths 1s, 2s, ..., 10s
      -- and alphabet sizes 2 and 4 respectively
      newSuffixOfAt2 = newSuffixOfBenchmarkList2 (Proxy @2) powersOf2
      newSuffixOfAt4 = newSuffixOfBenchmarkList2 (Proxy @4) powersOf2
