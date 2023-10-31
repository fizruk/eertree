{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Simple where

import           Control.DeepSeq             (NFData)
import           Data.Char                   (digitToInt)
import qualified Data.Foldable               as F
import           Data.List                   (maximumBy, nub)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Ord                    (comparing)
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           Data.String                 (IsString (..))
import           GHC.Generics                (Generic)
import           GHC.TypeLits                (KnownNat)

import           Control.Monad.ST            (runST)
import qualified Data.Vector.Unboxed         as UVector
import qualified Data.Vector.Unboxed.Mutable as MVector

import           EERTREE.Node
import           EERTREE.Symbol

-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XOverloadedStrings

-- | A palindromic tree for some string with auxillary information.
data EERTREE n = EERTREE
  { strLen            :: !Int           -- ^ Length of the analysed string.
  , maxPrefix         :: Node n         -- ^ Maximum palindromic prefix.
  , maxSuffix         :: Node n         -- ^ Maximum palindromic suffix
  , strReversedPrefix :: Seq (Symbol n) -- ^ Prefix, preceding maximum palindromic suffix
  , strSuffix         :: Seq (Symbol n) -- ^ Suffix, following maximum palindromic prefix.
  , palindromes       :: [Node n]       -- ^ Accumulated list of encountered palindromes.
  } deriving (Eq, Show, Generic)
instance NFData (EERTREE n)

instance KnownNat n => IsString (EERTREE n) where
  fromString = eertreeFromString

-- | An empty eertree.
empty :: forall n. KnownNat n => EERTREE n
empty = EERTREE
  { strLen             = 0
  , maxPrefix          = evenNode @n
  , maxSuffix          = evenNode @n
  , strReversedPrefix  = Seq.empty
  , strSuffix          = Seq.empty
  , palindromes        = []
  }

-- | An eertree for a singleton string.
--
-- >>> fromEERTREE (singleton @2 0)
-- [0]
singleton :: KnownNat n => Symbol n -> EERTREE n
singleton c = prepend c empty

-- | Analyse a string by building an eertree.
--
-- >>> fromEERTREE (eertree @2 [0,1,0,0,1])
-- [0,1,0,0,1]
eertree :: KnownNat n => [Symbol n] -> EERTREE n
eertree = foldr prepend empty

-- | Build an eertree from string
--
-- >>> eertreeFromString @2 "01001" == eertree @2 [0,1,0,0,1]
-- True
eertreeFromString :: KnownNat n => String -> EERTREE n
eertreeFromString = foldr (prepend . Symbol . charToInt) empty

-- | EERTREE of a reversed string
--
-- >> reverseEERTREE @2 "01001" == [1,0,0,1,0]
-- True
reverseEERTREE :: KnownNat n => EERTREE n -> EERTREE n
reverseEERTREE t = t { maxPrefix          = maxSuffix t
                     , maxSuffix          = maxPrefix t
                     , strReversedPrefix  = strSuffix t
                     , strSuffix          = strReversedPrefix t
                     }

-- | Get the string back from an eertree.
--
-- >>> fromEERTREE @2 "01001"
-- [0,1,0,0,1]
fromEERTREE :: EERTREE n -> [Symbol n]
fromEERTREE = F.toList . eertreeToSeq

eertreeToSeq :: EERTREE n -> Seq (Symbol n)
eertreeToSeq t = value (maxPrefix t) <> strSuffix t

-- | Get the reversed string from an eertree
--
-- >>> reverseFromEERTREE @2 "01001"
-- [1,0,0,1,0]
reverseFromEERTREE :: EERTREE n -> [Symbol n]
reverseFromEERTREE = F.toList . eertreeToSeqReversed

eertreeToSeqReversed :: EERTREE n -> Seq (Symbol n)
eertreeToSeqReversed t = value (maxSuffix t) <> strReversedPrefix t

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
--
-- >>> fromEERTREE (prepend 0 (eertreeFromString @2 "01001"))
-- [0,0,1,0,0,1]
prepend :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
prepend c t =
  case Seq.viewl (strSuffix t) of
    c' Seq.:< cs | c == c' -> EERTREE
      { strLen             = strLen t + 1
      , maxPrefix          = newMaxPrefix
      , maxSuffix          = if null cs then newMaxPrefix else maxSuffix t
      , strReversedPrefix  = if null cs then Seq.empty else strReversedPrefix t Seq.|> c
      , strSuffix          = cs
      , palindromes        = newMaxPrefix : palindromes t
      } where
        newMaxPrefix = edge c (maxPrefix t)
    _ ->
      case newSuffixOf c (maxPrefix t) of
        newMaxPrefix -> EERTREE
          { strLen             = strLen t + 1
          , maxPrefix          = newMaxPrefix
          , maxSuffix          = if null newStrSuffix then newMaxPrefix else maxSuffix t
          , strReversedPrefix  = if null newStrSuffix then Seq.empty else strReversedPrefix t Seq.|> c
          , strSuffix          = newStrSuffix
          , palindromes        = newMaxPrefix : palindromes t
          } where
              newStrSuffix = Seq.drop (n - 1) (value (maxPrefix t)) <> strSuffix t
              n = len newMaxPrefix

-- | Add a symbol to the end of a string
-- corresponding to an eertree
--
-- >>> fromEERTREE (append 0 (eertreeFromString @2 "01001"))
-- [0,1,0,0,1,0]
append :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
append c t = reverseEERTREE (prepend c (reverseEERTREE t))

-- | Merge two eertrees in O(1) on average
--
-- >>> fromEERTREE (merge @2 "0110100" "11001001")
-- [0,1,1,0,1,0,0,1,1,0,0,1,0,0,1]
--
-- >>> fromEERTREE (merge @2 "10010011" "0010110")
-- [1,0,0,1,0,0,1,1,0,0,1,0,1,1,0]
merge :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
merge t1 t2
  | strLen t1 <= strLen t2 = mergeLeft  s1 t2 []
  | otherwise              = mergeRight t1 s2 []
    where
      -- | Centers of max suffix and prefix
      c1 = fromIntegral (strLen t1) - fromIntegral (len (maxSuffix t1)) / 2
      c2 = fromIntegral (strLen t1) + fromIntegral (len (maxPrefix t2)) / 2

      -- | Strings representing the eertrees
      s1 = eertreeToSeqReversed t1
      s2 = eertreeToSeq t2

      -- | Merge by prepending symbols to `t2`
      mergeLeft s1' t2' pals
        | null s1'           = t2'
        | c1 <= newPalCenter = mergeLeft cs (prepend c t2') (newPal : pals)
        | otherwise          = t2' { strLen             = strLen t1 + strLen t2
                                   , maxPrefix          = maxPrefix t1
                                   , strReversedPrefix  = strReversedPrefix t2' <> s1'
                                   , strSuffix          = strSuffix t1 <> eertreeToSeq t2
                                   , palindromes        = pals <> palindromes t1 <> palindromes t2
                                   }
        where
          -- | Symbol to prepend
          (cs, c) = case Seq.viewl s1' of
                      Seq.EmptyL  -> (Seq.empty, Symbol 0)
                      x Seq.:< xs -> (xs, x)

          -- | New palindrome
          newPal = case Seq.viewl (strSuffix t2') of
                     x Seq.:< _ | c == x -> edge c (maxPrefix t2')
                     _                   -> newSuffixOf c (maxPrefix t2')

          -- | Center of a new palindrome
          newPalCenter = fromIntegral (length s1') + fromIntegral (len newPal) / 2

      -- | Merge by appending symbols to `t1`
      mergeRight t1' s2' pals
        | null s2'           = t1'
        | newPalCenter <= c2 = mergeRight (append c t1') cs (newPal : pals)
        | otherwise          = t1' { strLen             = strLen t1 + strLen t2
                                   , maxSuffix          = maxSuffix t2
                                   , strReversedPrefix  = strReversedPrefix t2 <> eertreeToSeqReversed t1
                                   , strSuffix          = strSuffix t1' <> s2'
                                   , palindromes        = pals <> palindromes t1 <> palindromes t2
                                   }
        where
          -- | Symbol to append
          (c, cs) = case Seq.viewl s2' of
                      Seq.EmptyL  -> (Symbol 0, Seq.empty)
                      x Seq.:< xs -> (x, xs)

          -- | New palindrome
          newPal = case Seq.viewl (strReversedPrefix t1') of
                     x Seq.:< _ | c == x -> edge c (maxSuffix t1')
                     _                   -> newSuffixOf c (maxSuffix t1')

          -- | Center of a new palindrome
          newPalCenter = fromIntegral (strLen t1') - fromIntegral (len newPal) / 2

-- | Merge two eertrees in min(|S1|, |S2|)
--
-- >>> fromEERTREE (mergeLinear @2 "0110100" "11001001")
-- [0,1,1,0,1,0,0,1,1,0,0,1,0,0,1]
--
-- >>> fromEERTREE (mergeLinear @2 "10010011" "0010110")
-- [1,0,0,1,0,0,1,1,0,0,1,0,1,1,0]
mergeLinear :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
mergeLinear t1 t2
  | strLen t1 < strLen t2 = foldr prepend t2 s1
  | otherwise             = foldr append t1 s2
    where
      s1 = eertreeToSeq t1
      s2 = eertreeToSeqReversed t2

-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes :: KnownNat n => [Symbol n] -> [[Symbol n]]
subpalindromes = map (F.toList . value) . nub . palindromes . eertree

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

-- | Palindromic refrain:
-- for a given string S find a palindrome P maximizing the value
-- |P| * occ(S, P), where occ(S, P) is the number of occurences of P in S
--
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

-- | Compute the number of occurences for each subpalindrome
--
-- >>> frequency (eertreeFromString @2 "10101")
-- [(fromPalindrome [1,0,1,0,1],1),(fromPalindrome [0,1,0],1),(fromPalindrome [1,0,1],2),(fromPalindrome [1],3),(fromPalindrome [0],2)]
frequency :: KnownNat n => EERTREE n -> [(Node n, Int)]
frequency = Map.elems . frequency'

-- | Compute the number of occurences for each subpalindrome.
-- Return @Map@ of frequences with Node index as a key.
--
-- >>> frequency' (eertreeFromString @2 "1000")
-- fromList [(-4,(fromPalindrome [0,0,0],1)),(-3,(fromPalindrome [1],1)),(-2,(fromPalindrome [0],3)),(2,(fromPalindrome [0,0],2))]
frequency' :: KnownNat n => EERTREE n -> Map Integer (Node n, Int)
frequency' t = Map.fromListWith combine pals
  where
    unfoldLinks = takeWhile (\n -> len n > 0) . iterate link

    -- Zip palindromes with their indexes
    pals = map ((\(p, f) -> (index p, (p, f))) . (\n -> (n, 1))) (concatMap unfoldLinks (palindromes t))

    combine pair1 pair2 = (fst pair1, snd pair1 + snd pair2)

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
