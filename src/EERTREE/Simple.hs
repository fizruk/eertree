{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Simple where

import           Data.Char                   (digitToInt)
import           Data.Foldable               (Foldable(toList))
import           Data.List                   (nub)
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           GHC.TypeLits                (KnownNat)

import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as MVector

import           EERTREE.Node
import           EERTREE.Symbol
import Debug.Trace

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

-- | A palindromic tree for some string with auxillary information.
data EERTREE n = EERTREE
  { strLen      :: !Int           -- ^ Length of the analysed string.
  , maxPrefix   :: Node n         -- ^ Maximum palindromic prefix.
  , maxSuffix   :: Node n         -- ^ Maximum palindromic suffix
  , strPrefix   :: Seq (Symbol n) -- ^ Prefix, preceding maximum palindromic suffix
  , strSuffix   :: Seq (Symbol n) -- ^ Suffix, following maximum palindromic prefix.
  , palindromes :: Seq (Node n)       -- ^ Accumulated list of encountered palindromes.
  } deriving (Show)

-- | An empty eertree.
empty :: forall n. KnownNat n => EERTREE n
empty = EERTREE
  { strLen      = 0
  , maxPrefix   = evenNode @n
  , maxSuffix   = evenNode @n
  , strPrefix   = Seq.empty
  , strSuffix   = Seq.empty
  , palindromes = Seq.empty
  }

-- | An eertree for a singleton string.
singleton :: KnownNat n => Symbol n -> EERTREE n
singleton c = prepend c empty

-- | Analyse a string by building an eertree.
eertree :: KnownNat n => Seq (Symbol n) -> EERTREE n
eertree = foldr prepend empty

eertreeFromList :: KnownNat n => [Symbol n] -> EERTREE n
eertreeFromList = foldr prepend empty

eertreeFromString :: KnownNat n => String -> EERTREE n
eertreeFromString s = eertreeFromList (map (Symbol . digitToInt) s)

-- | Get the string back from an eertree.
fromEERTREE :: EERTREE n -> Seq (Symbol n)
fromEERTREE t = value (maxPrefix t) <> strSuffix t

-- | Get the reversed string from an eertree
reverseFromEERTREE :: EERTREE n -> Seq (Symbol n)
reverseFromEERTREE t = value (maxSuffix t) <> reversedStrPrefix
  where
    reversedStrPrefix = reverseSeq (strPrefix t)

    -- | Reverse a sequence
    reverseSeq :: Seq (Symbol m) -> Seq (Symbol m)
    reverseSeq s = case Seq.viewr s of
      Seq.EmptyR    -> Seq.empty
      (xs Seq.:> x) -> x Seq.<| reverseSeq xs

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
prepend :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
prepend c t =
  case Seq.viewl (strSuffix t) of
    c' Seq.:< cs | c == c' -> EERTREE
      { strLen = strLen t + 1
      , maxPrefix = newMaxPrefix
      , maxSuffix = if null cs then newMaxPrefix else maxSuffix t
      , strPrefix = if null cs then cs else c Seq.<| strPrefix t
      , strSuffix = cs
      , palindromes = newMaxPrefix Seq.<| palindromes t
      } where
        newMaxPrefix = edge c (maxPrefix t)
    _ ->
      case newSuffixOf c (maxPrefix t) of
        newMaxPrefix -> EERTREE
          { strLen = strLen t + 1
          , maxPrefix = newMaxPrefix
          , maxSuffix = if null newStrSuffix then newMaxPrefix else maxSuffix t
          , strPrefix = if null newStrSuffix then newStrSuffix else c Seq.<| strPrefix t
          , strSuffix = newStrSuffix
          , palindromes = newMaxPrefix Seq.<| palindromes t
          } where
            newStrSuffix =
              let n = len newMaxPrefix
                in Seq.drop n (c Seq.<| value (maxPrefix t)) <> strSuffix t


-- | Add a symbol to the end of a string
-- corresponding to an eertree
append :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
append c t =
  case Seq.viewr (strPrefix t) of
    cs Seq.:> c' | c == c' -> EERTREE
      { strLen = strLen t + 1
      , maxPrefix = if null cs then newMaxSuffix else maxPrefix t
      , maxSuffix = newMaxSuffix
      , strPrefix = cs
      , strSuffix = if null cs then cs else strSuffix t Seq.|> c
      , palindromes = newMaxSuffix Seq.<| palindromes t
      } where
        newMaxSuffix = edge c (maxSuffix t)
    _ ->
      case newSuffixOf c (maxSuffix t) of
        newMaxSuffix -> EERTREE
          { strLen = strLen t + 1
          , maxPrefix = if null newStrPrefix then newMaxSuffix else maxPrefix t
          , maxSuffix = newMaxSuffix
          , strPrefix = newStrPrefix
          , strSuffix = if null newStrPrefix then newStrPrefix else strSuffix t Seq.|> c
          , palindromes = newMaxSuffix Seq.<| palindromes t
          } where
            newStrPrefix =
              let n = len newMaxSuffix
                in Seq.take (strLen t + 1 - n) (value (maxPrefix t) <> strSuffix t Seq.|> c)

-- | Merge two eertrees in O(1) amortized
-- mergeLeft
-- merge (eertreeFromString @2 "0110100") (eertreeFromString @2 "11001001")
-- merge (eertreeFromString @2 "100") (eertreeFromString @2 "0000")
-- merge (eertreeFromString @2 "1010") (eertreeFromString @2 "0000000101")
-- mergeRight:
-- merge (eertreeFromString @2 "10010011") (eertreeFromString @2 "0010110")
-- merge (eertreeFromString @2 "100") (eertreeFromString @2 "000")
merge :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
merge t1 t2
  | strLen t1 < strLen t2 = mergeLeft s1 t2 Seq.empty
  | otherwise             = mergeRight t1 s2 Seq.empty
    where
      -- | Centers of max suffix and prefix
      c1 = fromIntegral (strLen t1) - fromIntegral (len (maxSuffix t1)) / 2
      c2 = fromIntegral (strLen t1) + fromIntegral (len (maxPrefix t2)) / 2

      -- | Values of each tree
      s1 = strPrefix t1 <> value (maxSuffix t1)
      s2 = value (maxPrefix t2) <> strSuffix t2

      pals1 = palindromes t1
      pals2 = palindromes t2

      -- mergeLeft :: KnownNat m => Seq (Symbol m) -> EERTREE m -> Seq (Node m) -> EERTREE m
      mergeLeft s1' t2' pals
        | trace (show c1 ++ "-" ++ show newPalCenter ++ "-" ++ show c2 ++ "    " ++ show s1' ++ "    " ++ show (fromEERTREE t2') ++ "    " ++ show newPal ++ "    " ++ show cs ++ "    " ++ show c) (null s1')            = t2'
        | c1 <= newPalCenter = mergeLeft cs (prepend c t2') (newPal Seq.<| pals)
        | otherwise          = t2' { palindromes = pals1 <> pals2 <> pals }
        where
          -- | Symbol to prepend
          (cs, c) = case Seq.viewr s1' of
            Seq.EmptyR  -> (Seq.empty, Symbol 0)
            xs Seq.:> x -> (xs, x)

          -- | New palindrome
          newPal = case Seq.viewl (strSuffix t2') of
            x Seq.:< _ | c == x -> edge c (maxSuffix t2')
            _                   -> newSuffixOf c (maxPrefix t2')

          -- | Center of a new palindrome
          newPalCenter = fromIntegral (length s1') + fromIntegral (len newPal) / 2

      -- mergeRight :: KnownNat m => EERTREE m -> Seq (Symbol m) -> Seq (Node m) -> EERTREE m
      mergeRight t1' s2' pals
        | trace (show c1 ++ "-" ++ show newPalCenter ++ "-" ++ show c2 ++ "    " ++ show (fromEERTREE t1') ++ "    " ++ show s2' ++ "    " ++ show newPal ++ "    " ++ show c ++ "    " ++ show cs) (null s2')            = t1'
        | newPalCenter <= c2 = mergeRight (append c t1') cs (newPal Seq.<| pals)
        | otherwise          = t1' { palindromes = pals1 <> pals2 <> pals }
        where
          -- | Symbol to append
          (c, cs) = case Seq.viewl s2' of
            Seq.EmptyL  -> (Symbol 0, Seq.empty)
            x Seq.:< xs -> (x, xs)

          -- | New palindrome
          newPal = case Seq.viewr (strPrefix t1') of
            _ Seq.:> x | c == x -> edge c (maxSuffix t1')
            _                   -> newSuffixOf c (maxSuffix t1')

          -- | Center of a new palindrome
          newPalCenter = fromIntegral (strLen t1') - fromIntegral (len newPal) / 2

-- | Merge two eertrees in min(|S1|, |S2|)
mergeLinear :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
mergeLinear t1 t2
  | strLen t1 < strLen t2 = foldr prepend t2 s1
  | otherwise             = foldr append t1 s2
    where
      s1 = fromEERTREE t1
      s2 = reverseFromEERTREE t2

-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes :: KnownNat n => Seq (Symbol n) -> [Seq (Symbol n)]
subpalindromes = map value . nub . (toList . palindromes) . eertree

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
