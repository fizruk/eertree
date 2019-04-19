{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Double where

import           Data.List                   (nub)
import           Data.Ord                    (comparing)
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
  , maxSuffix   :: Node n     -- ^ Maximum palindromic suffix.
  , maxPrefix   :: Node n     -- ^ Maximum palindromic prefix.
  , strSuffix   :: [Symbol n] -- ^ Suffix, following maximum palindromic prefix.
  , strPrefix   :: [Symbol n] -- ^ Prefix (reversed), preceding maximum palindromic suffix.
  , suffixPals  :: [Node n]   -- ^ Accumulated list of encountered suffix palindromes.
  , prefixPals  :: [Node n]   -- ^ Accumulated list of encountered prefix palindromes.
  } deriving (Show)

instance Eq (EERTREE n) where
  x == y = (x `compare` y) == EQ

instance Ord (EERTREE n) where
  compare = comparing strLen
          <> comparing maxSuffix
          <> comparing maxPrefix
          <> comparing strSuffix
          <> comparing strPrefix
          <> comparing suffixPals
          <> comparing prefixPals

-- | An empty eertree.
empty :: forall n. KnownNat n => EERTREE n
empty = EERTREE
  { strLen      = 0
  , maxSuffix   = evenNode @n
  , maxPrefix   = evenNode @n
  , strSuffix   = []
  , strPrefix   = []
  , suffixPals  = []
  , prefixPals  = []
  }

-- | An eertree for a singleton string.
singleton :: KnownNat n => Symbol n -> EERTREE n
singleton c = prepend c empty

-- | Analyse a string by building an eertree using prepend.
--
-- prop> eertree @2 (listMod xs) == eertree' (listMod xs)
eertree :: KnownNat n => [Symbol n] -> EERTREE n
eertree s = foldr prepend empty s

-- | Analyse a string by building an eertree using append.
--
-- prop> eertree @2 (listMod xs) == eertree' (listMod xs)
eertree' :: KnownNat n => [Symbol n] -> EERTREE n
eertree' s =  foldl (flip append) empty s

-- | Combine two eertrees.
--
-- prop> merge @2 (eertree (listMod xs)) (eertree (listMod ys)) == eertree (listMod (xs ++ ys))
-- prop> merge @2 (eertree' (listMod xs)) (eertree' (listMod ys)) == eertree (listMod (xs ++ ys))
merge :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
merge l r
  | strLen l > strLen r = mergeToLeft l r
  | otherwise           = mergeToRight l r

-- | Combine two eertrees (suffix nature) in a smart way
--
-- prop> mergeToLeft @2 (eertree' (listMod xs)) (eertree' (listMod ys)) == eertree' (listMod (xs ++ ys))
mergeToLeft :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
mergeToLeft l r =
  let t = foldl (flip append) l (value (maxPrefix r))
  in addLeft t (strSuffix r)
  where
    addLeft t []       = t
    addLeft t s@(c:cs) =
      case strPrefix t of
        c':_ | c' == c -> addLeft (append c t) cs
        _    | len (newSuffixOf c (maxSuffix t)) > (strLen r) - (length cs) 
                       -> addLeft (append c t) cs
        _              -> t { strLen      = strLen t + (length s)
                            , maxSuffix   = maxSuffix r
                            , strSuffix   = drop (len (maxPrefix t)) (fromEERTREE l ++ fromEERTREE r)
                            , strPrefix   = reverse (fromEERTREE l ++ reverse (strPrefix r))
                            , suffixPals  = take ((strLen r) - ((strLen t) - (strLen l))) (suffixPals r) ++ suffixPals t
                            , prefixPals  = take ((strLen r) - ((strLen t) - (strLen l))) (prefixPals r) ++ prefixPals t
                            }

-- | Combine two eertrees (prefix nature) in a smart way
--
-- prop>  mergeToRight @2 (eertree (listMod xs)) (eertree (listMod ys)) == eertree (listMod (xs ++ ys))
mergeToRight :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
mergeToRight l r =
  let t = foldr prepend r (value (maxSuffix l))
  in addRight t (strPrefix l)
  where
    addRight t []       = t
    addRight t s@(c:cs) =
      case strSuffix t of
        c':_ | c' == c -> addRight (prepend c t) cs
        _    | len (newSuffixOf c (maxPrefix t)) > (strLen l) - (length cs) 
                       -> addRight (prepend c t) cs
        _              -> t { strLen      = strLen t + (length s)
                            , maxPrefix   = maxPrefix l
                            , strPrefix   = drop (len (maxSuffix t)) (reverse (fromEERTREE l ++ fromEERTREE r))
                            , strSuffix   = strSuffix l ++ fromEERTREE r
                            , suffixPals  = take ((strLen l) - ((strLen t) - (strLen r))) (suffixPals l) ++ suffixPals t
                            , prefixPals  = take ((strLen l) - ((strLen t) - (strLen r))) (prefixPals l) ++ prefixPals t
                            }

-- | Pop last symbol from eertree (suffix nature)
--
-- prop>  (length xs > 0) ==> popBack @2 (eertree' (listMod xs)) == eertree' (init (listMod (xs)))
popBack :: KnownNat n => EERTREE n -> EERTREE n
popBack t
  | strLen t == 0 = error "eertree is already empty"
  | strLen t == 1 = empty
  | otherwise     = t { strLen      = strLen t - 1
                      , maxPrefix   = oldMaxPrefix 
                      , maxSuffix   = head oldPalindromes
                      , strPrefix   = reverse (take ((strLen t) - len (head oldPalindromes) - 1) (fromEERTREE t))
                      , strSuffix   = drop (len oldMaxPrefix) (init(fromEERTREE t))
                      , suffixPals  = oldPalindromes 
                      }
  where
    oldPalindromes = tail (suffixPals t)
    oldPrefixPals  = tail (_)
    oldMaxPrefix   = head oldPrefixPals
 
-- | Pop last symbol from eertree (prefix nature)
--
-- prop>  (length xs > 0) ==> popFront @2 (eertree (listMod xs)) == eertree (tail (listMod (xs)))
popFront :: KnownNat n => EERTREE n -> EERTREE n
popFront t
  | strLen t == 0 = error "eertree is already empty"
  | strLen t == 1 = empty
  | otherwise     = t { strLen      = strLen t - 1
                      , maxPrefix   = head oldPalindromes
                      , maxSuffix   = oldMaxSuffix 
                      , strPrefix   = reverse (take ((strLen t) - len (oldMaxSuffix) - 1) (tail(fromEERTREE t)))
                      , strSuffix   = drop (len (head oldPalindromes)) (tail(fromEERTREE t))
                      , prefixPals  = oldPalindromes 
                      }
  where
    oldPalindromes = tail (prefixPals t)
    oldMaxSuffix
      | len (maxSuffix t) == strLen t = maximum(map ((flip directLink) (maxSuffix t)) alphabet)
      | otherwise                     = maxSuffix t 

-- | Get the string back from an eertree.
fromEERTREE :: EERTREE n -> [Symbol n]
fromEERTREE t = value (maxPrefix t) ++ strSuffix t

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
prepend :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
prepend c t = checkSuffix $
  case strSuffix t of
    c':cs | c == c' -> t
      { strLen = strLen t + 1
      , maxPrefix = edge c (maxPrefix t)
      , strPrefix = strPrefix t ++ [c]
      , strSuffix = cs
      , prefixPals = edge c (maxPrefix t) : prefixPals t
      }
    _ ->
      case newSuffixOf c (maxPrefix t) of
        newMaxPrefix -> t
          { strLen = strLen t + 1
          , maxPrefix = newMaxPrefix
          , strPrefix = strPrefix t ++ [c]
          , strSuffix =
              let n = len newMaxPrefix
               in drop n (c : value (maxPrefix t)) ++ strSuffix t
          , prefixPals = newMaxPrefix : prefixPals t
          }
  where
    checkSuffix t' -- TODO Here should be updating mechanism
      | len (maxPrefix t') == strLen t' = t' { strPrefix = [], maxSuffix = maxPrefix t', suffixPals = suffixPals (eertree' (fromEERTREE t')) }
      | otherwise                       = t' { suffixPals = suffixPals (eertree' (fromEERTREE t')) }  

-- | Add a symbol to the end of a string
-- corresponding to an eertree.
append :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
append c t = checkPrefix $
  case strPrefix t of
    c':cs | c == c' -> t
      { strLen = strLen t + 1
      , maxSuffix = edge c (maxSuffix t)
      , strPrefix = cs
      , strSuffix = strSuffix t ++ [c]
      , suffixPals = edge c (maxSuffix t) : suffixPals t
      }
    _ ->
      case newSuffixOf c (maxSuffix t) of
        newMaxSuffix -> t
          { strLen = strLen t + 1
          , maxSuffix = newMaxSuffix
          , strPrefix =
              let n = len (maxSuffix t) - len newMaxSuffix + 1
               in reverse (take n (value (maxSuffix t))) ++ strPrefix t
          , strSuffix = strSuffix t ++ [c]
          , suffixPals = newMaxSuffix : suffixPals t
          }
  where
    checkPrefix t' -- TODO Here should be updating mechanism
      | len (maxSuffix t') == strLen t' = t' { strSuffix = [], maxPrefix = maxSuffix t', prefixPals = prefixPals (eertree (fromEERTREE t'))}
      | otherwise                       = t' { prefixPals = prefixPals (eertree (fromEERTREE t')) }


-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes :: KnownNat n => [Symbol n] -> [[Symbol n]]
subpalindromes = map value . nub . prefixPals . eertree

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
  , maxPrefix t' `notElem` prefixPals t
  ]
