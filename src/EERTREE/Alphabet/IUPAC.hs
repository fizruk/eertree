{-# LANGUAGE DataKinds #-}
module EERTREE.Alphabet.IUPAC where

import           Data.String            (IsString (..))
import           EERTREE.Alphabet.Class

-- $setup
-- >>> :set -XOverloadedStrings

type SymbolRNA = PalindromeAlphabet (FixedAlphabet ["A","C","G","U"])

type SymbolDNA = PalindromeAlphabet (FixedAlphabet ["A","C","G","T"])

newtype RNA = RNA { getRNA :: [SymbolRNA] }
  deriving (Eq, Ord)

instance Show RNA where
  show = concatMap show . getRNA

-- |
-- >>> "AAGCU" :: RNA
-- AAGCU
instance IsString RNA where
  fromString = RNA . map (\c -> fromString [c])

newtype DNA = DNA { getDNA :: [SymbolDNA] }

instance Show DNA where
  show = concatMap show . getDNA

-- |
-- >>> "AAGCT" :: DNA
-- AAGCT
instance IsString DNA where
  fromString = DNA . map (\c -> fromString [c])

