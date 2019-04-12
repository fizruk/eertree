{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module EERTREE.Mergeable where

import           GHC.TypeLits   (KnownNat)

import           EERTREE.Node
import           EERTREE.Simple (EERTREE (..))
import qualified EERTREE.Simple as Simple
import           EERTREE.Symbol

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

-- | A palindromic tree for some string with auxillary information.
data M n = M
  { forward  :: EERTREE n
  , backward :: EERTREE n
  } deriving (Eq, Ord, Show)

empty :: KnownNat n => M n
empty = M Simple.empty Simple.empty

rightToLeft :: KnownNat n => [Symbol n] -> M n
rightToLeft = foldr prepend empty

leftToRight :: KnownNat n => [Symbol n] -> M n
leftToRight = foldl (flip append) empty

fromM :: M n -> [Symbol n]
fromM = Simple.fromEERTREE . forward

{-# RULES "reverseM/reverseM"
    reverseM . reverseM = id #-}
reverseM :: M n -> M n
reverseM (M f b) = M b f

prepend :: KnownNat n => Symbol n -> M n -> M n
prepend c M{..} = M forward' backward'
  where
    isWholePalindrome = len (maxPrefix forward') == strLen forward'
    forward' = Simple.prepend c forward
    backward' = updateBackwards $ backward
      { strLen = 1 + strLen backward
      , palindromes =
          updatePalindromes len id
            (strLen forward')
            (palindromes backward)
            newPalindromes
          ++ [edge c oddNode]
      }

    updateBackwards b
      | isWholePalindrome = b { maxPrefix = maxPrefix forward', strSuffix = [] }
      | otherwise         = b { strSuffix = strSuffix backward ++ [c] }

    newPalindromes = map (edge c)
      (Simple.palPrefixesBefore c forward)

append :: KnownNat n => Symbol n -> M n -> M n
append c = reverseM . prepend c . reverseM

popLeft :: KnownNat n => M n -> Maybe (Symbol n, M n)
popLeft m@M{..} =
  case Simple.pop forward of
    Nothing            -> Nothing
    Just (c, forward') -> Just (c, M forward' (go c forward'))
  where
    go c forward' = backward'
      where
        isWholePalindromeRemoved = len (maxPrefix forward) == strLen forward
        backward' = updateBackwards $ backward
          { strLen = strLen backward - 1
          , palindromes =
              updatePalindromes (\n -> len n - 1) link
                (strLen forward')
                (init (palindromes backward))
                newPalindromes
          }

        newMaxSuffix
          | null (palindromes backward') = evenNode
          | otherwise = head (palindromes backward')

        updateBackwards b
          | isWholePalindromeRemoved
              = b { maxPrefix = newMaxSuffix
                  , strSuffix = take (strLen forward' - len newMaxSuffix) (Simple.fromEERTREE forward') }
          | otherwise = b { strSuffix = init (strSuffix backward) }

        link = maximum . links    -- FIXME: uneffective

        newPalindromes = map (edge c)
          (Simple.palPrefixesBefore c forward')

-- |
-- >>> updatePalindromes id 10 [0,0,0,0,0,0,0,0,0,0] [5,3]
-- [0,0,0,0,0,5,0,3,0,0]
updatePalindromes :: (b -> Int) -> (b -> a) -> Int -> [a] -> [b] -> [a]
updatePalindromes _ _ _ xs [] = xs
updatePalindromes _ _ _  [] _  = []
updatePalindromes l f k xs (n:ns)
  = pre ++ f n : updatePalindromes l f (l n - 1) (drop 1 post) ns
  where
    (pre, post) = splitAt (k - l n) xs
