-- GHC 7.6.3 
-- Simplified solution for https://timus.online/problem.aspx?space=1&num=1297
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Data.Bits    (clearBit)
import           Data.Char    (ord, chr)
import           Data.IntMap  (IntMap)
import qualified Data.IntMap  as IntMap
import           Data.List    (maximumBy, nub, unfoldr)
import           Data.Maybe   (fromMaybe)
import           Data.Monoid  ((<>))
import           Data.Ord     (comparing)

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

alphabetSize :: Int
alphabetSize = 58

-- | A symbol in an alphabet of size @n@.
newtype Symbol n = Symbol { fromSymbol :: Int }
  deriving (Eq, Ord, Show, Enum)

instance  Bounded (Symbol n) where
  minBound = Symbol 0
  maxBound = Symbol (alphabetSize - 1)

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
alphabet :: forall n.  [Symbol n]
alphabet = [minBound..maxBound]

-- | Convert a list of integers into a list of 'Symbol's
-- using @'mod' n@.
--
-- >>> listMod @5 [0..20]
-- [0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0]
listMod ::  [Int] -> [Symbol n]
listMod = map (\k -> Symbol (k `mod` n))
  where
    n = 57
type Vector = []

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

-- | A node corresponsing to a palindrome
-- in an alphabet of size @n@.
data Node n = Node
  { len       :: !Int
  , parent    :: Maybe (Symbol n, Node n)
  , ancestors :: Vector (Node n)
  , edges     :: IntMap (Node n)
  , links     :: IntMap (Node n)
  }

fromWeakly = id
applyWeakly = id

(!?) :: [a] -> Int -> Maybe a
xs !? i = lookup i (zip [0..] xs)

-- | Nodes are compared by length first and by half
-- of the palindrome they represent next.
instance Eq (Node n) where
  t1 == t2 = t1 `compare` t2 == EQ

-- | Nodes are compared by length first and by half
-- of the palindrome they represent next.
instance Ord (Node n) where
  compare = comparing len <> comparing pathTo

instance Show (Node n) where
  show node
    | len node < 0 = "oddNode"
    | otherwise    = "fromPalindrome " ++ show (value node)

-- | First (last) symbol of a palindrome.
--
-- >>> symbol (fromPalindrome @2 [1,0,1])
-- Just 1
-- >>> symbol (fromPalindrome @2 [])
-- Nothing
symbol :: Node n -> Maybe (Symbol n)
symbol = fmap fst . parent

-- | Follow an edge in palindromic tree.
-- This corresponds to adding a symbol to both ends of
-- some palindrome.
--
-- >>> edge 1 (fromPalindrome @2 [0,1,0])
-- fromPalindrome [1,0,1,0,1]
edge :: Symbol n -> Node n -> Node n
edge c node = fromWeakly (edges node IntMap.! fromSymbol c)

-- | Follow a direct link to find the largest palindrome
-- suffix preceded by a given symbol in another palindrome.
--
-- >>> directLink 1 (fromPalindrome @2 [0,1,0,1,0])
-- fromPalindrome [0,1,0]
-- >>> directLink 0 (fromPalindrome @2 [0,1,0,1,0])
-- fromPalindrome []
-- >>> directLink 1 (fromPalindrome @2 [0,0,0,0,0])
-- oddNode
directLink :: Symbol n -> Node n -> Node n
directLink c node = fromMaybe oddNode (IntMap.lookup (fromSymbol c) (links node))

-- | Construct a @'Node' n@ from a palindrome.
--
-- NOTE: this will result in an error if input is not a palindrome.
--
-- >>> fromPalindrome @2 [0,1,1]
-- *** Exception: not a palindrome
-- ...
fromPalindrome :: [Symbol n] -> Node n
fromPalindrome xs
  | not isPalindrome = error "not a palindrome"
  | even n    = foldr edge evenNode half
  | otherwise = foldr edge oddNode  half
  where
    n = length xs
    half = take ((n + 1) `div` 2) xs
    isPalindrome = xs == reverse xs

-- | Like 'symbolAt'', but tries to minimize jumps.
--
-- >>> symbolAt 3 (fromPalindrome @2 [0,1,0,1,0,1,0])
-- Just 1
symbolAt :: Int -> Node n -> Maybe (Symbol n)
symbolAt i t = symbolAt' (min i (len t - i - 1)) t

-- | Find a symbol at a given position in a palindrome.
-- Note that it does not matter from what end you start indexing.
symbolAt' :: Int -> Node n -> Maybe (Symbol n)
symbolAt' i _ | i < 0 = Nothing
symbolAt' 0 t = symbol t
symbolAt' i t = do
  ancestor <- ancestors t !? n
  symbolAt' (i `clearBit` n) ancestor
  where
    n = countTrailingZeros i

countTrailingZeros :: Int -> Int
countTrailingZeros n = length (takeWhile (== 0) (map (`mod` 2) (iterate (`div` 2) n)))

-- | Path to the node from one of the roots.
-- This corresponds to the left half of the palindrome
-- a node represents.
--
-- >>> pathTo (fromPalindrome @2 [1,1,0,1,1])
-- [1,1,0]
pathTo :: Node n -> [Symbol n]
pathTo = unfoldr parent

-- | Value of a node (its palindrome).
--
-- >>> value (fromPalindrome @2 [1,1,0,1,1])
-- [1,1,0,1,1]
value :: Node n -> [Symbol n]
value t
  | even (len t) = s <> reverse s
  | otherwise    = s <> drop 1 (reverse s)
  where
    s = pathTo t

-- | Build a 'Vector' of a node's parent ancestors
-- that are $2^i$ edges away from the node.
--
-- >>> value <$> getAncestors (fromPalindrome @2 [1,0,1,0,1,0,1])
-- [[1,0,1,0,1,0,1],[0,1,0,1,0],[0]]
getAncestors :: Node a -> Vector (Node a)
getAncestors = go 0 . Just
  where
    go _ Nothing  = []
    go i (Just t) = t : go (i + 1) (ancestors t !? i)

-- | Construct a new node by following an edge.
-- This corresponds to adding a symbol to both ends of a palindrome.
mkEdge :: Symbol n -> Node n -> Node n
mkEdge c parentNode = t
  where
    t = Node
      { len    = len parentNode + 2
      , parent = Just (c, parentNode)
      , ancestors = getAncestors parentNode
      , edges  = IntMap.fromList [ (fromSymbol c', applyWeakly (mkEdge c') t) | c' <- alphabet ]
      , links  = mkDirectLinks c parentNode
      }

-- | Find what would be the largest suffix of a new palindrome
-- after added a given symbol to the end of another palindrome.
--
-- >>> newSuffixOf 1 (fromPalindrome @2 [1,0,1,0,1])
-- fromPalindrome [1,1]
-- >>> newSuffixOf 0 (fromPalindrome @2 [1,0,1,0,1])
-- fromPalindrome [0,1,0,1,0]
-- >>> newSuffixOf 1 (fromPalindrome @2 [0,0,0,0,0])
-- fromPalindrome [1]
newSuffixOf :: Symbol n -> Node n -> Node n
newSuffixOf c = edge c . directLink c

-- | Construct direct links for a child node,
-- given its parent and a symbol to add to both ends.
--
-- >>> mkDirectLinks 0 (fromPalindrome @2 [0,1,0,1,0])
-- fromList [(0,fromPalindrome [0]),(1,fromPalindrome [0,0])]
-- >>> mkDirectLinks 1 (fromPalindrome @2 [0,1,0,1,0])
-- fromList [(0,fromPalindrome [1,0,1,0,1]),(1,fromPalindrome [])]
mkDirectLinks
  :: Symbol n        -- ^ Symbol to add.
  -> Node n          -- ^ Parent palindrome.
  -> IntMap (Node n) -- ^ Direct links for child palindrome.
mkDirectLinks c parentNode =
  case symbolAt (len newMaxSuf) t of
    Just c'
      | otherwise  -> IntMap.insert (fromSymbol c') newMaxSuf (links newMaxSuf)
    Nothing
      | len t == len newMaxSuf -> IntMap.singleton (fromSymbol c) evenNode
      | otherwise -> links newMaxSuf
  where
    newMaxSuf = newSuffixOf c parentNode
    t = fromWeakly (edges parentNode IntMap.! fromSymbol c)

-- | An even node corresponding to an empty palindrome.
--
-- >>> evenNode @2
-- fromPalindrome []
-- >>> edge 1 (evenNode @2)
-- fromPalindrome [1,1]
evenNode ::  Node n
evenNode = Node
  { len   = 0
  , parent = Nothing
  , ancestors = []
  , edges = IntMap.fromList [ (fromSymbol c, applyWeakly (mkEdge c) evenNode) | c <- alphabet ]
  , links = IntMap.fromList [ (fromSymbol c, oddNode) | c <- alphabet  ]
  }

-- | An odd node, corresponding to a parent of singleton palindromes.
--
-- >>> edge 1 (oddNode @2)
-- fromPalindrome [1]
--
-- Note that there is no palindrome that corresponds to this node:
--
-- >>> oddNode @2
-- oddNode
oddNode = Node
  { len   = -1
  , parent = Nothing
  , ancestors = []
  , edges = IntMap.fromList [ (fromSymbol c, applyWeakly (mkEdge c) oddNode) | c <- alphabet ]
  , links = IntMap.fromList [ (fromSymbol c, oddNode) | c <- alphabet  ]
  }

-- | A palindromic tree for some string with auxillary information.
data EERTREE n = EERTREE
  { strLen      :: !Int       -- ^ Length of the analysed string.
  , maxPrefix   :: Node n     -- ^ Maximum palindromic prefix.
  , strSuffix   :: [Symbol n] -- ^ Suffix, following maximum palindromic prefix.
  , palindromes :: [Node n]   -- ^ Accumulated list of encountered palindromes.
  } deriving (Show)

-- | An empty eertree.
empty :: EERTREE n
empty = EERTREE
  { strLen      = 0
  , maxPrefix   = evenNode 
  , strSuffix   = []
  , palindromes = []
  }

-- | An eertree for a singleton string.
singleton :: Symbol n -> EERTREE n
singleton c = prepend c empty

-- | Analyse a string by building an eertree.
eertree :: [Symbol n] -> EERTREE n
eertree = foldr prepend empty

-- | Get the string back from an eertree.
fromEERTREE :: EERTREE n -> [Symbol n]
fromEERTREE t = value (maxPrefix t) ++ strSuffix t

-- | Add a symbol to the beginning of a string
-- corresponding to an eertree.
prepend :: Symbol n -> EERTREE n -> EERTREE n
prepend c t =
  case strSuffix t of
    c':cs | c == c' -> EERTREE
      { strLen = strLen t + 1
      , maxPrefix = edge c (maxPrefix t)
      , strSuffix = cs
      , palindromes = edge c (maxPrefix t) : palindromes t
      }
    _ ->
      case newSuffixOf c (maxPrefix t) of
        newMaxPrefix -> EERTREE
          { strLen = strLen t + 1
          , maxPrefix = newMaxPrefix
          , strSuffix =
              let n = len newMaxPrefix
               in drop n (c : value (maxPrefix t)) ++ strSuffix t
          , palindromes = newMaxPrefix : palindromes t
          }

-- * Applications

-- | Unique subpalindromes of a string.
--
-- >>> subpalindromes @2 [0,1,0,0,1]
-- [[0,1,0],[1,0,0,1],[0,0],[0],[1]]
subpalindromes ::  [Symbol n] -> [[Symbol n]]
subpalindromes = map value . nub . palindromes . eertree

numpal ::  [Symbol n] -> Int
numpal = sum . map (length . suffixes) . palindromes . eertree
  where
    suffixes :: Node n -> [Node n]
    suffixes node
      | len node <= 0 = []
      | otherwise = node : suffixes maxsuf
        where
          maxsuf = maximumBy (comparing len) (IntMap.elems (links node))

fromString :: String -> [Symbol n]
fromString = map fromChar
  where
    fromChar c = Symbol (ord c - ord 'A')

fromSymbolList :: [Symbol n] -> String
fromSymbolList = map fromOrd
  where
    fromOrd c = chr(fromSymbol c + ord 'A')

main :: IO ()
main = do
  s <- getLine
  putStrLn (fromSymbolList(value(maximum(palindromes (eertree (fromString s))))))
