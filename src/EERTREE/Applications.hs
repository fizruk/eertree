{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Applications where

import           Control.DeepSeq              (force)
import           Control.Monad                (replicateM)
import           Control.Monad.ST             (runST)
import           Control.Parallel.Strategies
import           Data.Hashable
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HashSet
import           Data.List                    (maximumBy)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Ord                     (comparing)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Vector.Unboxed          as UVector
import qualified Data.Vector.Unboxed.Mutable  as MVector
import           GHC.TypeLits                 (KnownNat)
import           Math.NumberTheory.Logarithms (intLog2')

import qualified EERTREE.List
import           EERTREE.Node                 (Node)
import qualified EERTREE.Node                 as Node
import           EERTREE.Simple
import           EERTREE.Symbol

-- | Compute first \(n\) elements of <https://oeis.org/A216264 A216264 sequence>
-- (binary rich strings count for \(n = 0, 1, \ldots\)).
--
-- For memory efficiency the whole sequence segment
-- is computed at once and not lazily as one might expect.
--
-- This should run in \(\mathcal{O}(n)\) memory
-- with garbage collector working normally.
--
-- >>> a216264 15
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
      [ dfsCountLevels (n - startLength) (initEERTREEwithHashSet (eertree (0 : startBits))) (richSubEERTREEs' @2)
      | startBits <- replicateM (startLength - 1) [0,1]   -- only works for startLength <= 7, since not all binary strings are rich starting from n = 8
      ] `using` parList rpar

a216264' :: Int -> [Int]
a216264' n = 1 : map (*2) halves
  where
    halves = dfsCountLevels (n - 1) (initEERTREEwithHashSet (singleton 0)) (richSubEERTREEs' @2)

initEERTREEwithHashSet :: EERTREE n -> (HashSet (Node n), EERTREE n)
initEERTREEwithHashSet t = (HashSet.fromList (palindromes t), t)

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
    refrain = map (\(p, f) -> f * Node.len p) (Map.elems occ)

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

instance Hashable (Node n) where
  hashWithSalt salt = hashWithSalt salt . Node.index

-- | Efficiently compute all rich eertrees that can be produced
-- from a given one by prepending a symbol to it.
--
-- Efficiency comes from the fact that we only need to check
-- that a new maximum prefix is a new palindrome.
richSubEERTREEs' :: forall n. KnownNat n => (HashSet (Node n), EERTREE n) -> [(HashSet (Node n), EERTREE n)]
richSubEERTREEs' (ps, t) =
  [ (HashSet.insert newPalindrome ps, t')
  | c <- alphabet @n
  , let t' = prepend c t
  , let newPalindrome = maxPrefix t'
  , not (newPalindrome `HashSet.member` ps)
  ]

-- Return the length of the longest palindromic substring
-- and the number of substrings with such length.
maxSubLengthSum :: [(Node n, Int)] -> (Int, Int)
maxSubLengthSum listFreq =
  let
    maxLength = maximumBy (comparing (Node.len . fst)) listFreq
    -- Get the nodes that have the maximum length only
    maxNodes = filter (\(node, _) -> Node.len node == Node.len (fst maxLength)) listFreq
    freqValues = map snd maxNodes
  in
    (Node.len (fst maxLength), sum freqValues)

getMsubstrPar :: Int -> Text -> (Int, Int)
getMsubstrPar cores input = (lenn, freq)
  where
    chunkSize = Text.length input `div` cores
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
