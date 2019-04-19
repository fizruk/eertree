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
-- Can be refered as double-ended eertree
data M n = M
  { forward  :: EERTREE n -- ^ Eertree for prefixes.
  , backward :: EERTREE n -- ^ Eertree for suffixes.
  } deriving (Eq, Ord, Show)

-- | An empty double-ended eertree.
--
-- >>> empty @2
-- M 
--   { forward = EERTREE 
--     { strLen = 0
--     , maxPrefix = fromPalindrome []
--     , strSuffix = []
--     , palindromes = []
--     }
--   , backward = EERTREE 
--     { strLen = 0
--     , maxPrefix = fromPalindrome []
--     , strSuffix = []
--     , palindromes = []
--     }
--   }
empty :: KnownNat n => M n
empty = M Simple.empty Simple.empty

-- | Reverse double-ended eertree.
--
-- >>> fromM (reverseM (leftToRight  @2 [1, 0, 0, 0]))
-- [0,0,0,1]
{-# RULES 
  "reverseM/reverseM" reverseM . reverseM = id 
#-}
reverseM :: M n -> M n
reverseM (M f b) = M b f

-- | Analyse a string by building an double-ended eertree using prepend.
--
-- >>> rightToLeft @2 [1, 0, 0, 0]
-- M 
--  { forward = EERTREE 
--    { strLen      = 4
--    , maxPrefix   = fromPalindrome [1] 
--    , strSuffix   = [0,0,0]
--    , palindromes = [fromPalindrome [1],fromPalindrome [0,0,0],fromPalindrome [0,0],fromPalindrome [0]]
--    }
--  , backward = EERTREE 
--    { strLen      = 4
--    , maxPrefix   = fromPalindrome [0,0,0]
--    , strSuffix   = [1]
--    , palindromes = [fromPalindrome [0,0,0],fromPalindrome [0,0],fromPalindrome [0],fromPalindrome [1]]
--    }
--  }
rightToLeft :: KnownNat n => [Symbol n] -> M n
rightToLeft = foldr prepend empty

-- | Analyse a string by building an double-ended eertree using append.
--
-- >>> leftToRight  @2 [1, 0, 0, 0]
-- M 
--  { forward = EERTREE 
--    { strLen = 4
--    , maxPrefix = fromPalindrome [1]
--    , strSuffix = [0,0,0]
--    , palindromes = [fromPalindrome [1],fromPalindrome [0,0,0],fromPalindrome [0,0],fromPalindrome [0]]
--    }
--  , backward = EERTREE 
--    { strLen = 4
--    , maxPrefix = fromPalindrome [0,0,0]
--    , strSuffix = [1]
--    , palindromes = [fromPalindrome [0,0,0],fromPalindrome [0,0],fromPalindrome [0],fromPalindrome [1]]
--    }
--  }
--
--  prop> leftToRight @2 (listMod xs) == rightToLeft (listMod xs)
leftToRight :: KnownNat n => [Symbol n] -> M n
leftToRight = foldl (flip append) empty

-- | Get the string back from an double-ended eertree.
--
-- >>> fromM (leftToRight  @2 [1, 0, 0, 0])
-- [1,0,0,0]
fromM :: M n -> [Symbol n]
fromM = Simple.fromEERTREE . forward

-- | Add a symbol to the beginning of a string
-- corresponding to an double-ended eertree.
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

-- | Add a symbol to the end of a string
-- corresponding to an double-ended eertree.
append :: KnownNat n => Symbol n -> M n -> M n
append c = reverseM . prepend c . reverseM

-- | Remove symbol at the beginning of a string
-- corresponding to an double-ended eertree.
--
-- prop> (length xs > 2) ==> popLeft @2 (leftToRight (listMod xs)) == Just(head (fromM(leftToRight (listMod xs))),(leftToRight (tail (listMod (xs)))))
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
                  , strSuffix = (drop (len newMaxSuffix) (init(Simple.fromEERTREE backward))) }
          | otherwise = b { strSuffix = init (strSuffix backward) }

        link = maximum . links    -- FIXME: uneffective

        newPalindromes = map (edge c)
          (Simple.palPrefixesBefore c forward')

-- | Remove symbol at the end of a string
-- corresponding to an double-ended eertree.
--
--
-- prop> (length xs > 2) ==> popRight @2 (leftToRight (listMod xs)) == Just(last (fromM(leftToRight (listMod xs))),(leftToRight (init (listMod (xs)))))
popRight :: KnownNat n => M n -> Maybe (Symbol n, M n)
popRight t = 
  case popLeft(reverseM t) of 
    Nothing     -> Nothing
    Just (c, t') -> Just (c, (reverseM t'))

merge :: KnownNat n => M n -> M n -> M n
merge l r
  | strLen (forward l) > strLen (forward r) = mergeToLeft l r
  | otherwise                               = mergeToRight l r


mergeToLeft :: KnownNat n => M n -> M n -> M n
mergeToLeft l r =
  let t = foldl (flip append) l (value (maxPrefix (forward r)))
  in addLeft t (strSuffix (forward r))
  where  
    addLeft t []       = t
    addLeft t s@(c:cs) = 
      case strSuffix (backward t) of
        c':_ | c' == c -> addLeft (append c t) cs
        _    | len (newSuffixOf c (maxPrefix (backward t))) > (strLen (forward r)) - (length cs)  -> addLeft (append c t) cs
        _ -> t -- TODO: simple gluing

mergeToRight :: KnownNat n => M n -> M n -> M n
mergeToRight l r = 
  let t = foldr prepend r (value (maxPrefix (backward l)))
  in addRight t (strSuffix (backward l))
  where
    addRight t []       = t
    addRight t s@(c:cs) =
      case strSuffix (forward t) of
        c':_ | c' == c -> addRight (prepend c t) cs
        _    | len (newSuffixOf c (maxPrefix (forward t))) > (strLen (forward l)) - (length cs) -> addRight (prepend c t) cs
        _  -> t -- TODO: simple gluing

-- | Update list with given function to determine position at which it should be replaced
--
-- >>> updatePalindromes id id 10 [0,0,0,0,0,0,0,0,0,0] [5,3]
-- [0,0,0,0,0,5,0,3,0,0]
updatePalindromes :: (b -> Int) -> (b -> a) -> Int -> [a] -> [b] -> [a]
updatePalindromes _ _ _ xs [] = xs
updatePalindromes _ _ _  [] _  = []
updatePalindromes l f k xs (n:ns)
  = pre ++ f n : updatePalindromes l f (l n - 1) (drop 1 post) ns
  where
    (pre, post) = splitAt (k - l n) xs
