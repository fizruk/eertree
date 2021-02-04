{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit    (fromHUnitTest)
import           Test.HUnit

import           Data.List                   (intercalate)
import qualified Data.Set                    as S
import           GHC.TypeLits                (KnownNat)

import EERTREE.Simple

main :: IO ()
main = hspec $ do
  describe "merge tests" $ do
    fromHUnitTest mergeTestSuite

-- | Testing merge
mergeTestSuite :: Test
mergeTestSuite = TestList
  [ TestLabel "merge_both_empty"    test_merge_both_empty
  , TestLabel "merge_first_empty"   test_merge_first_empty
  , TestLabel "merge_second_empty"  test_merge_second_empty
  , TestLabel "merge_both_nonempty" test_merge_both_nonempty
  ]

test_merge_both_empty :: Test
test_merge_both_empty = TestCase $ do
  merge t1 t2 @?= empty
  where
    t1 = eertreeFromString @2 ""
    t2 = eertreeFromString @2 ""

test_merge_first_empty :: Test
test_merge_first_empty = TestCase $ do
  all (\t -> merge t1 t == t) t2 @?= True
  where
    t1 = eertreeFromString @3 ""
    t2 = map (eertreeFromString @3) testStrings

test_merge_second_empty :: Test
test_merge_second_empty = TestCase $ do
  all (\t -> merge t t2 == t) t1 @?= True
  where
    t1 = map (eertreeFromString @3) testStrings
    t2 = eertreeFromString @3 ""

test_merge_both_nonempty :: Test
test_merge_both_nonempty = TestCase $ do
  and [ compareEERTREEs
          (merge (eertreeFromString @3 x)
                 (eertreeFromString @3 y))
          (eertreeFromString @3 (x ++ y))
        | x <- s1, y <- s2 ] @?= True
  where
    s1 = testStrings
    s2 = s1

testStrings :: [String]
testStrings = generateStrings 3 5

-- | Generate eertrees from all strings of alphabet size @n@ and of length @m@
-- generateEERTREEs :: Int -> Int -> [EERTREE]
-- generateEERTREEs n m = map (eertreeFromString @n) (generateStrings n m)

-- | Generate all strings of alphabet size @n@ and of length @m@
--
-- >>> generateStrings 2 3
-- ["","0","1","00","10","01","11"]
generateStrings :: Int -> Int -> [String]
generateStrings 0 _ = []
generateStrings _ 0 = []
generateStrings n m = take (n^m - 1) buildStrings
  where
    buildStrings = [] : [ x : xs | xs <- buildStrings, x <- intercalate "" alpha ]
    alpha = map show [0 .. n-1]

-- | Compare EERTREEs
compareEERTREEs :: KnownNat n => EERTREE n -> EERTREE n -> Bool
compareEERTREEs t1 t2 =
  maxPrefix t1 == maxPrefix t2 &&
  maxSuffix t1 == maxSuffix t2 &&
  strReversedPrefix t1 == strReversedPrefix t2 &&
  strSuffix t1 == strSuffix t2 &&
  comparePalindromes (palindromes t1) (palindromes t2)
  where
    comparePalindromes pals1 pals2 = S.fromList pals1 == S.fromList pals2
