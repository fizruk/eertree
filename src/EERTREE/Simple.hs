{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Simple where

import           Data.List                   (nub)
import           GHC.TypeLits                (KnownNat)

import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as MVector

import           EERTREE.Node
import           EERTREE.Symbol

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

-- | A palindromic tree for some string with auxillary information.
data EERTREE n = EERTREE
  { strLen      :: !Int       -- ^ Length of the analysed string.
  , maxPrefix   :: Node n     -- ^ Maximum palindromic prefix.
  , maxSuffix   :: Node n     -- ^ Maximum palindromic suffix
  , strPrefix   :: [Symbol n] -- ^ Prefix, preceding maximum palindromic suffix
  , strSuffix   :: [Symbol n] -- ^ Suffix, following maximum palindromic prefix.
  , palindromes :: [Node n]   -- ^ Accumulated list of encountered palindromes.
  } deriving (Show)

-- | An empty eertree.
empty :: forall n. KnownNat n => EERTREE n
empty = EERTREE
  { strLen      = 0
  , maxPrefix   = evenNode @n
  , maxSuffix   = evenNode @n
  , strPrefix   = []
  , strSuffix   = []
  , palindromes = []
  }

-- | An eertree for a singleton string.
singleton :: KnownNat n => Symbol n -> EERTREE n
singleton c = prepend c empty

-- | Analyse a string by building an eertree.
eertree :: KnownNat n => [Symbol n] -> EERTREE n
eertree = foldr prepend empty

-- | Get the string back from an eertree.
fromEERTREE :: EERTREE n -> [Symbol n]
fromEERTREE t = value (maxPrefix t) ++ strSuffix t

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
prepend :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
prepend c t =
  case strSuffix t of
    c':cs | c == c' -> EERTREE
      { strLen = strLen t + 1
      , maxPrefix = newMaxPrefix
      , maxSuffix = if null cs then newMaxPrefix else maxSuffix t
      , strPrefix = if null cs then cs else c : strPrefix t
      , strSuffix = cs
      , palindromes = newMaxPrefix : palindromes t
      } where
        newMaxPrefix = edge c (maxPrefix t)
    _ ->
      case newSuffixOf c (maxPrefix t) of
        newMaxPrefix -> EERTREE
          { strLen = strLen t + 1
          , maxPrefix = newMaxPrefix
          , maxSuffix = if null newStrSuffix then newMaxPrefix else maxSuffix t
          , strPrefix = if null newStrSuffix then newStrSuffix else c : strPrefix t
          , strSuffix = newStrSuffix
          , palindromes = newMaxPrefix : palindromes t
          } where
            newStrSuffix =
              let n = len newMaxPrefix
                in drop n (c : value (maxPrefix t)) ++ strSuffix t


-- | Add a symbol to the end of a string
-- corresponding to an eertree
append :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
append c t =
  case strPrefix t of
    c':cs | c == c' -> EERTREE
      { strLen = strLen t + 1
      , maxPrefix = if null cs then newMaxSuffix else maxPrefix t
      , maxSuffix = newMaxSuffix
      , strPrefix = cs
      , strSuffix = if null cs then cs else strSuffix t ++ [c]
      , palindromes = newMaxSuffix : palindromes t
      } where
        newMaxSuffix = edge c (maxSuffix t)
    _ ->
      case newSuffixOf c (maxSuffix t) of
        newMaxSuffix -> EERTREE
          { strLen = strLen t + 1
          , maxPrefix = if null newStrPrefix then newMaxSuffix else maxPrefix t
          , maxSuffix = newMaxSuffix
          , strPrefix = newStrPrefix
          , strSuffix = if null newStrPrefix then newStrPrefix else strSuffix t ++ [c]
          , palindromes = newMaxSuffix : palindromes t
          } where
            newStrPrefix =
              let n = len newMaxSuffix
                in take (strLen t + 1 - n) (value (maxPrefix t) ++ strSuffix t ++ [c])


-- | Merge two eertrees
merge :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
merge t1 t2
  | strLen t1 < strLen t2 = foldr prepend t2 s1
  | otherwise             = foldr append t1 s2
    where
      s1 = fromEERTREE t1
      s2 = reverse (fromEERTREE t2)

-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes :: KnownNat n => [Symbol n] -> [[Symbol n]]
subpalindromes = map value . nub . palindromes . eertree

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
a216264 :: Int -> [Int]
a216264 n = 1 : map (*2) halves
  where
    -- Observation: there is exactly the same number of rich strings
    -- that start with 0 as there are those starting with 1.
    -- That is why we can do half work (or 1/(alphabet size) in general)
    -- and count rich strings faster.
    halves = dfsCountLevels (n - 1) (singleton 0) (richSubEERTREEs @2)

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
      go v i t
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
