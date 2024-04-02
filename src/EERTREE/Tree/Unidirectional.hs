{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Tree.Unidirectional where

import           Control.DeepSeq (NFData)
import qualified Data.Foldable   as F
import           Data.String     (IsString (..))
import           Data.List                   (nub)
import           GHC.Generics    (Generic)
import           GHC.TypeLits    (KnownNat)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           EERTREE.Node
import           EERTREE.Symbol

-- $setup
-- >>> :set -XTypeApplications -XDataKinds -XOverloadedStrings

-- | A single-ended version of a palindromic tree.
data EERTREE n = EERTREE
  { strLen      :: !Int       -- ^ Length of the analysed string.
  , maxPrefix   :: Node n     -- ^ Maximum palindromic prefix.
  , maxSuffix   :: Node n     -- ^ Maximum palindromic suffix
  , strSuffix   :: [Symbol n] -- ^ Suffix, following maximum palindromic prefix.
  , palindromes :: [Node n]   -- ^ Accumulated list of encountered palindromes.
  } deriving (Eq, Show, Generic)

instance NFData (EERTREE n)

instance KnownNat n => IsString (EERTREE n) where
  fromString = eertreeFromString

-- | An empty eertree.
empty :: forall n. KnownNat n => EERTREE n
empty = EERTREE
  { strLen      = 0
  , maxPrefix   = evenNode
  , maxSuffix   = evenNode
  , strSuffix   = []
  , palindromes = []
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

-- | Get the string back from an eertree.
--
-- >>> fromEERTREE @2 "01001"
-- [0,1,0,0,1]
fromEERTREE :: EERTREE n -> [Symbol n]
fromEERTREE t = (F.toList . value) (maxPrefix t) ++ strSuffix t

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
--
-- >>> fromEERTREE (prepend 0 (eertreeFromString @2 "01001"))
-- [0,0,1,0,0,1]
prepend :: KnownNat n => Symbol n -> EERTREE n -> EERTREE n
prepend c t =
  case strSuffix t of
    c' : cs | c == c' -> EERTREE
      { strLen      = strLen t + 1
      , maxPrefix   = newMaxPrefix
      , maxSuffix   = if null cs then newMaxPrefix else maxSuffix t
      , strSuffix   = cs
      , palindromes = newMaxPrefix : palindromes t
      } where
        newMaxPrefix = edge c (maxPrefix t)
    _ -> EERTREE
      { strLen      = strLen t + 1
      , maxPrefix   = newMaxPrefix
      , maxSuffix   = if null newStrSuffix then newMaxPrefix else maxSuffix t
      , strSuffix   = newStrSuffix
      , palindromes = newMaxPrefix : palindromes t
      } where
        newStrSuffix = drop (n - 1) ((F.toList . value) (maxPrefix t)) ++ strSuffix t
        n = len newMaxPrefix
        newMaxPrefix = newSuffixOf c (maxPrefix t)

-- | Merge two eertrees in O(1) on average
--
-- >>> fromEERTREE (merge @2 "0110100" "11001001")
-- [0,1,1,0,1,0,0,1,1,0,0,1,0,0,1]
merge :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
merge t1 t2 = mergeLeft s1 t2 []
  where
    s1 = fromEERTREE t1

    maxSuffixC = fromIntegral (strLen t1) - fromIntegral (len (maxSuffix t1)) / 2

    mergeLeft s1' t2' pals
      | null s1'              = t2'
      | maxSuffixC <= newPalC = mergeLeft cs (prepend c t2') (newPal : pals)
      | otherwise             = t2' { strLen      = strLen t1 + strLen t2
                                    , maxPrefix   = maxPrefix t1
                                    , strSuffix   = strSuffix t1 <> fromEERTREE t2
                                    , palindromes = pals <> palindromes t1 <> palindromes t2
                                    }
        where
          (cs, c) = case s1' of
                      x : xs -> (xs, x)
                      []     -> ([], Symbol 0)

          newPal = case strSuffix t2' of
                     x : _ | c == x -> edge c (maxPrefix t2')
                     _              -> newSuffixOf c (maxPrefix t2')

          newPalC = fromIntegral (length s1') + fromIntegral (len newPal) / 2

-- | Merge two eertrees in O(|t1|)
--
-- >>> fromEERTREE (mergeLinear @2 "0110100" "11001001")
-- [0,1,1,0,1,0,0,1,1,0,0,1,0,0,1]
mergeLinear :: KnownNat n => EERTREE n -> EERTREE n -> EERTREE n
mergeLinear t1 t2 = foldr prepend t2 s1
  where
    s1 = fromEERTREE t1

-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes :: KnownNat n => [Symbol n] -> [[Symbol n]]
subpalindromes = map (F.toList . value) . nub . palindromes . eertree

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
