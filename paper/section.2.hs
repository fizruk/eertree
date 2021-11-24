import           Data.List   (drop, tails)
import           Data.Maybe  (fromMaybe, listToMaybe)
import           Text.Printf (printf)

type Palindrome = String

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

edge :: Char -> Palindrome -> Palindrome
edge x s = x : s ++ [x]

directLink :: Char -> String -> Maybe Palindrome
directLink c s = listToMaybe [ p | c':p <- tails s, c' == c, isPalindrome p ]

newMaxSuffix :: Char -> Palindrome -> Palindrome
newMaxSuffix c p = case maybeDirectLink of
  Just suffix -> edge c suffix
  Nothing     -> [c]
  where
    maybeDirectLink = directLink c p

data PalString = PalString {
  value         :: String,
  reversePrefix :: String,
  maxSuffix     :: Palindrome,
  palindromes   :: [Palindrome]
} deriving Show

append :: Char -> PalString -> PalString
append c ps = case reversePrefix ps of
  c_:cs -> PalString {
    value = newValue,
    reversePrefix = newReversePrefix,
    maxSuffix = newPalindrome,
    palindromes = palindromes ps ++ [newPalindrome]
  } where
    newValue = value ps ++ [c]
    newPalindrome =
      if c_ == c
      then edge c (maxSuffix ps)
      else newMaxSuffix c (maxSuffix ps)

    newReversePrefix =
      if c_ == c
      then cs
      else reverse (take (length (maxSuffix ps) - length (newPalindrome) + 1) (maxSuffix ps)) ++ (reversePrefix ps)

  []    -> PalString {
    value = newValue,
    reversePrefix = newReversePrefix,
    maxSuffix = newPalindrome,
    palindromes = palindromes ps ++ [newPalindrome]
  } where
    newValue = value ps ++ [c]
    newPalindrome = newMaxSuffix c (value ps)
    newReversePrefix = reverse (take (length newValue - length newPalindrome) (newValue))

build :: String -> PalString
build s = build_ s empty
  where
  empty = PalString { value = "", reversePrefix = "", maxSuffix = "", palindromes = [] }

build_ :: String -> PalString -> PalString
build_ (c:cs) ps = build_ cs (append c ps)
build_ [] ps     = ps

main :: IO()
main = do
  let s = "aababaaba"

  printf "string = \"%s\"\n" s
  printf "directLink 'a' \"%s\" = %s\n" s (show (directLink 'a' s))

  printf "newMaxSuffix 'a' \"%s\" = \"%s\"\n" s (newMaxSuffix 'a' s)
  printf "newMaxSuffix 'b' \"%s\" = \"%s\"\n" s (newMaxSuffix 'b' s)
  printf "newMaxSuffix 'c' \"%s\" = \"%s\"\n" s (newMaxSuffix 'c' s)

  let example = build s
  printf "PalString of \"%s\" = %s\n" s (show (example))
  printf "append 'a' \"%s\" = %s\n" s (show (append 'a' example))
  printf "append 'b' \"%s\" = %s\n" s (show (append 'b' example))
