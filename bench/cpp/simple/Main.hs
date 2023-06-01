
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
module Main where

import qualified Data.ByteString       as BS
import           Data.ByteString.UTF8  as BSU (fromString)
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.Proxy            (Proxy (..))
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.TypeLits          (KnownNat, natVal)
import qualified Language.C.Inline.Cpp as C

import           EERTREE.Random
import           EERTREE.Symbol

import           Criterion.Main
import           Test.QuickCheck

C.context (C.cppCtx <> C.bsCtx)
C.include "eertree.cpp"

-- | Build eertree from a given list of symbols
-- and show its @maxSuffix@
benchRandomEERTREE :: BS.ByteString -> IO CString
benchRandomEERTREE s = output
  where
    output = [C.block|
      const char* {
      EERTREE tree;
      for (int i = 0; i < $bs-len:s; ++i)
        tree.insert($bs-ptr:s, i);
      // tree.printAll($bs-ptr:s);
      return tree.palindromesAll($bs-ptr:s).c_str();
      }
    |]

-- | For a given alphabet size and a list of eertree lengths
-- construct a list of corresponding eertree benchmarks
eertreeBenchmarkList :: forall n. KnownNat n => Proxy n -> [Int] -> [Benchmark]
eertreeBenchmarkList _ lens = [ env (randomSymbolsIO @n len)
                                (\s ->
                                  bgroup ("eertree @" ++ show (natVal (Proxy @n)))
                                  [ bench ("len " ++ show len) $ whnfAppIO benchRandomEERTREE (BSU.fromString (symbolsToLetters s)) ]
                                )
                              | len <- lens ]

symbolsToLetters :: KnownNat n => [Symbol n] -> String
symbolsToLetters = map (alphabet Map.!)
  where
    alphabet = Map.fromList $ zip [0 ..] ['a' .. 'z']

main :: IO ()
main = defaultMain
  ( eertreeAt2 ++ eertreeAt4
  )
    where
      s = 10

      -- | List of 1s, 2s, 4s, 8s, and 16s
      powersOf2 = take 5 [ s * 2^x | x <- [0..] ]

      -- | Lists of benchmarks for eertrees of lenths 1s, 2s, 4s, 8s, and 16s
      -- and alphabet sizes 2 and 4 respectively
      eertreeAt2 = eertreeBenchmarkList (Proxy @2) powersOf2
      eertreeAt4 = eertreeBenchmarkList (Proxy @4) powersOf2
