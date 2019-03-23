{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module EERTREE.Symbol where

import           Data.Proxy
import           GHC.TypeLits (KnownNat, Nat, natVal)

-- | A symbol in an alphabet of size @n@.
newtype Symbol (n :: Nat) = Symbol { fromSymbol :: Int }
  deriving newtype (Eq, Ord, Show, Enum)

instance KnownNat n => Bounded (Symbol n) where
  minBound = Symbol 0
  maxBound = Symbol (n - 1)
    where
      n = fromInteger (natVal (Proxy @n))

-- | This instance is used solely for convenience
-- of using integer literals for 'Symbol'.
-- None of the numeric operations are available except for 'fromInteger'.
instance Num (Symbol n) where
  fromInteger = Symbol . fromInteger
  (+)     = error "operation + is not defined for Symbol"
  (*)     = error "operation * is not defined for Symbol"
  abs     = error "operation abs is not defined for Symbol"
  signum  = error "operation signum is not defined for Symbol"
  negate  = error "operation negate is not defined for Symbol"
  (-)     = error "operation - is not defined for Symbol"

-- | Symbols of an alphabet of size @n@.
--
-- >>> alphabet @2
-- [0,1]
-- >>> alphabet @10
-- [0,1,2,3,4,5,6,7,8,9]
alphabet :: forall n. KnownNat n => [Symbol n]
alphabet = [minBound..maxBound]
