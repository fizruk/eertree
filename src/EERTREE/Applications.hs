{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns    #-}
module EERTREE.Applications where

import Control.DeepSeq (force)
import Control.Monad (replicateM)
import           Data.Ord                    (comparing)
import           Data.List                   (maximumBy)
import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as MVector
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           GHC.TypeLits                (KnownNat)
import           Control.Monad.ST            (runST)
import           Math.NumberTheory.Logarithms (intLog2')
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Parallel.Strategies

import EERTREE.Node ( Node(len) )
import EERTREE.Symbol
import           EERTREE.Simple

-- | Compute first \(n\) elements of <https://oeis.org/A216264 A216264 sequence>
-- (binary rich strings count for \(n = 0, 1, \ldots\)).
--
-- For memory efficiency the whole sequence segment
-- is computed at once and not lazily as one might expect.
--
-- This should run in \(\mathcal{O}(n)\) memory
-- with garbage collector working normally.
--
-- >>> a216264' 15
-- [1,2,4,8,16,32,64,128,252,488,932,1756,3246,5916,10618]
a216264 :: Int -> Int -> [Int]
a216264 cores n = a216264' startLength ++ map (*2) halves
  where
    -- Observation: there is exactly the same number of rich strings
    -- that start with 0 as there are those starting with 1.
    -- That is why we can do half work (or 1/(alphabet size) in general)
    -- and count rich strings faster.
    halves = foldr (zipWith (+)) (repeat 0) results
    startLength = min 7 (1 + intLog2' cores)
    results =
      [ dfsCountLevels (n - startLength) (eertree (0 : startBits)) (richSubEERTREEs @2)
      | startBits <- replicateM (startLength - 1) [0,1]   -- only works for startLength <= 7, since not all binary strings are rich starting from n = 8
      ] `using` parList rpar

a216264' :: Int -> [Int]
a216264' n = 1 : map (*2) halves
  where
    halves = dfsCountLevels (n - 1) (singleton 0) (richSubEERTREEs @2)

-- | Palindromic refrain:
-- for a given string S find a palindrome P maximizing the value
-- |P| * occ(S, P), where occ(S, P) is the number of occurences of P in S
--
-- >>> :set -XDataKinds
-- >>> palindromicRefrain (eertreeFromString @3 "0102010")
-- (fromPalindrome [0,1,0,2,0,1,0],7)
--
-- >>> palindromicRefrain (eertreeFromString @1 "000")
-- (fromPalindrome [0,0],4)
palindromicRefrain :: KnownNat n => EERTREE n -> (Node n, Int)
palindromicRefrain t = maximumBy (comparing snd) (zip unique refrain)
  where
    -- | Count palindrome frequency
    occ = frequency' t

    -- | Unique palindromes
    unique = map fst (Map.elems occ)

    -- | Calculate refrains
    refrain = map (\(p, f) -> f * len p) (Map.elems occ)

-- * Helpers

-- | Efficiently count nodes at every level of a tree using DFS.
--
-- Memory efficiency comes from the fact that in depth-first
-- traversal we don't have to store all nodes at a level
-- and only need to store a path from root to the current node.
dfsCountLevels
  :: Int          -- ^ Number of levels to cover (max depth).
  -> a            -- ^ Root.
  -> (a -> [a])   -- ^ How to get subtrees.
  -> [Int]        -- ^ Number of elements at every level.
dfsCountLevels k root subtrees = runST $ do
  result <- MVector.replicate k 0
  go result 0 root
  UVector.toList <$> UVector.freeze result
    where
      go v !i t
        | i >= k = return ()
        | otherwise = do
          MVector.modify v (+1) i
          mapM_ (go v (i+1)) (subtrees t)

-- | Efficiently compute all rich eertrees that can be produced
-- from a given one by prepending a symbol to it.
--
-- Efficiency comes from the fact that we only need to check
-- that a new maximum prefix is a new palindrome.
richSubEERTREEs :: forall n. KnownNat n => EERTREE n -> [EERTREE n]
richSubEERTREEs t =
  [ t'
  | c <- alphabet @n
  , let t' = prepend c t
  , maxPrefix t' `notElem` palindromes t
  ]

-- Return the length of the longest palindromic substring
-- and the number of substrings with such length.
maxSubLengthSum :: [(Node n, Int)] -> (Int, Int)
maxSubLengthSum listFreq =
  let
    maxLength = maximumBy (comparing (len . fst)) listFreq
    -- Get the nodes that have the maximum length only
    maxNodes = filter (\(node, _) -> len node == len (fst maxLength)) listFreq
    freqValues = map snd maxNodes
  in
    (len (fst maxLength), sum freqValues)

getMsubstrPar :: Int -> Text -> (Int, Int)
getMsubstrPar _chunkSize input = (lenn, freq)
  where
    chunkSize = Text.length input `div` 16
    tree = foldr merge empty chunks
    chunks =
      [ eertreeFromString @26 (Text.unpack chunk)
      | chunk <- Text.chunksOf chunkSize input
      ] `using` parList rpar
    (lenn, freq) = maxSubLengthSum (frequency tree)

getMsubstr :: String -> (Int, Int)
getMsubstr str = (lenn, freq)
  where
    tree = eertreeFromString @26 str
    (lenn, freq) = maxSubLengthSum (frequency tree)
