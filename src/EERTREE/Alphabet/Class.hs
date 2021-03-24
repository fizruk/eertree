{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module EERTREE.Alphabet.Class where

import           Data.Maybe     (fromMaybe)
import           Data.Proxy     (Proxy (..))
import           Data.String    (IsString (..))
import           EERTREE.Symbol (Symbol)
import           GHC.Exts       (Constraint)
import           GHC.TypeLits   (type (+), KnownNat, Nat, natVal)
import qualified GHC.TypeLits   as GHC

class (Ord a, KnownNat (AlphabetSize a)) => Alphabet a where
  type AlphabetSize a :: Nat
  toAlphabet :: Symbol (AlphabetSize a) -> a
  fromAlphabet :: a -> Symbol (AlphabetSize a)
  complementOf :: a -> a
  isComplementOf :: a -> a -> Bool

  isComplementOf x y = x == complementOf y

instance KnownNat n => Alphabet (Symbol n) where
  type AlphabetSize (Symbol n) = n
  toAlphabet = id
  fromAlphabet = id
  complementOf = id
  isComplementOf = (==)

newtype PalindromeAlphabet a = PalindromeAlphabet { unwrapPalindromeAlphabet :: a }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Enum, Num, IsString)

instance Alphabet a => Alphabet (PalindromeAlphabet a) where
  type AlphabetSize (PalindromeAlphabet a) = AlphabetSize a
  toAlphabet = PalindromeAlphabet . toAlphabet
  fromAlphabet = fromAlphabet . unwrapPalindromeAlphabet
  complementOf x = toAlphabet (toEnum (n - i - 1))
    where
      i = fromEnum (fromAlphabet x)
      n = fromInteger (natVal (Proxy @(AlphabetSize a)))

  isComplementOf x y = (i + j + 1) == n
    where
      i = fromEnum (fromAlphabet x)
      j = fromEnum (fromAlphabet y)
      n = fromInteger (natVal (Proxy @(AlphabetSize a)))

type family Length (xs :: [k]) where
  Length '[] = 0
  Length (x:xs) = 1 + Length xs

type family All c (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x:xs) = (c x, All c xs)

newtype FixedAlphabet (alphabet :: [GHC.Symbol]) = FixedAlphabet
  { unwrapFixedAlphabet :: Symbol (Length alphabet)
  } deriving newtype (Eq, Ord, Enum)

-- |
-- >>> [minBound .. maxBound] :: [FixedAlphabet ["A","B","C"]]
-- [A,B,C]
deriving instance KnownNat (Length alphabet) => Bounded (FixedAlphabet alphabet)

instance KnownNat (Length alphabet) => Alphabet (FixedAlphabet alphabet) where
  type AlphabetSize (FixedAlphabet alphabet) = Length alphabet
  toAlphabet = FixedAlphabet
  fromAlphabet = unwrapFixedAlphabet
  complementOf = toAlphabet . complementOf . fromAlphabet
  isComplementOf x y = fromAlphabet x `isComplementOf` fromAlphabet y

instance (KnownNat (Length alphabet), ListToValues String alphabet) => Show (FixedAlphabet alphabet) where
  show x = listToValues (Proxy @alphabet) !! i
    where
      i = fromEnum (fromAlphabet x)

instance (KnownNat (Length alphabet), ListToValues String alphabet) => IsString (FixedAlphabet alphabet) where
  fromString s = fromMaybe invalid (lookup s m)
    where
      invalid = error ("symbol " <> show s <> " is not a member of alphabet " <> show (listToValues @GHC.Symbol @String (Proxy @alphabet)))
      m = [ (show x, x) | x <- [minBound .. maxBound] ]

class Known a (x :: k) where
  toValue :: Proxy x -> a

instance KnownNat n => Known Integer n where
  toValue = natVal

instance GHC.KnownSymbol s => Known String s where
  toValue = GHC.symbolVal

class All (Known a) xs => ListToValues a (xs :: [k]) where
  listToValues :: Proxy xs -> [a]

instance ListToValues a '[] where
  listToValues _ = []

instance (Known a x, ListToValues a xs) => ListToValues a (x:xs) where
  listToValues _ = toValue (Proxy @x) : listToValues (Proxy @xs)

alphabetSize :: forall a. Alphabet a => Proxy a -> Integer
alphabetSize _ = natVal (Proxy @(AlphabetSize a))
