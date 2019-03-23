{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module EERTREE.Node where

import           Data.Bits                    (clearBit, countTrailingZeros)
import           Data.Coerce                  (coerce)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.List                    (unfoldr)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Ord                     (comparing)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           GHC.TypeLits                 (KnownNat, Nat)

import           EERTREE.Node.Internal.Weakly
import           EERTREE.Symbol

-- $setup
-- >>> :set -XTypeApplications -XDataKinds

-- | A node corresponsing to a palindrome
-- in an alphabet of size @n@.
data Node (n :: Nat) = Node
  { len       :: !Int
  , parent    :: Maybe (Symbol n, Node n)
  , ancestors :: Vector (Node n)
  , edges     :: IntMap (Weakly (Node n))
  , links     :: IntMap (Node n)
  }

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
edge c node = fromWeakly (edges node IntMap.! coerce c)

-- | Follow a direct link to find the largest palindrome
-- suffix preceded by a given symbol in another palindrome.
--
-- >>> directLink 1 (fromPalindrome @2 [0,1,0,1,0])
-- fromPalindrome [0,1,0]
-- >>> directLink 0 (fromPalindrome @2 [0,1,0,1,0])
-- fromPalindrome []
-- >>> directLink 1 (fromPalindrome @2 [0,0,0,0,0])
-- oddNode
directLink :: KnownNat n => Symbol n -> Node n -> Node n
directLink c node = fromMaybe oddNode (IntMap.lookup (coerce c) (links node))

-- | Construct a @'Node' n@ from a palindrome.
--
-- NOTE: this will result in an error if input is not a palindrome.
--
-- >>> fromPalindrome @2 [0,1,1]
-- *** Exception: not a palindrome
-- ...
fromPalindrome :: KnownNat n => [Symbol n] -> Node n
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
  ancestor <- ancestors t Vector.!? n
  symbolAt' (i `clearBit` n) ancestor
  where
    n = countTrailingZeros i

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
getAncestors = Vector.fromList . go 0 . Just
  where
    go _ Nothing  = []
    go i (Just t) = t : go (i + 1) (ancestors t Vector.!? i)

-- | Construct a new node by following an edge.
-- This corresponds to adding a symbol to both ends of a palindrome.
mkEdge :: KnownNat n => Symbol n -> Node n -> Node n
mkEdge c parentNode = t
  where
    t = Node
      { len    = len parentNode + 2
      , parent = Just (c, parentNode)
      , ancestors = getAncestors parentNode
      , edges  = IntMap.fromList [ (coerce c', applyWeakly (mkEdge c') t) | c' <- alphabet ]
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
newSuffixOf :: KnownNat n => Symbol n -> Node n -> Node n
newSuffixOf c = edge c . directLink c

-- | Construct direct links for a child node,
-- given its parent and a symbol to add to both ends.
--
-- >>> mkDirectLinks 0 (fromPalindrome @2 [0,1,0,1,0])
-- fromList [(0,fromPalindrome [0]),(1,fromPalindrome [0,0])]
-- >>> mkDirectLinks 1 (fromPalindrome @2 [0,1,0,1,0])
-- fromList [(0,fromPalindrome [1,0,1,0,1]),(1,fromPalindrome [])]
mkDirectLinks
  :: KnownNat n
  => Symbol n        -- ^ Symbol to add.
  -> Node n          -- ^ Parent palindrome.
  -> IntMap (Node n) -- ^ Direct links for child palindrome.
mkDirectLinks c parentNode =
  case symbolAt (len newMaxSuf) t of
    Just c'
      | otherwise  -> IntMap.insert (coerce c') newMaxSuf (links newMaxSuf)
    Nothing
      | len t == len newMaxSuf -> IntMap.singleton (coerce c) evenNode
      | otherwise -> links newMaxSuf
  where
    newMaxSuf = newSuffixOf c parentNode
    t = fromWeakly (edges parentNode IntMap.! coerce c)

-- | An even node corresponding to an empty palindrome.
--
-- >>> evenNode @2
-- fromPalindrome []
-- >>> edge 1 (evenNode @2)
-- fromPalindrome [1,1]
evenNode :: forall n. KnownNat n => Node n
evenNode = Node
  { len   = 0
  , parent = Nothing
  , ancestors = Vector.empty
  , edges = IntMap.fromList [ (coerce c, applyWeakly (mkEdge c) evenNode) | c <- alphabet ]
  , links = IntMap.fromList [ (coerce c, oddNode) | c <- alphabet @n ]
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
oddNode :: forall n. KnownNat n => Node n
oddNode = Node
  { len   = -1
  , parent = Nothing
  , ancestors = Vector.empty
  , edges = IntMap.fromList [ (coerce c, applyWeakly (mkEdge c) oddNode) | c <- alphabet ]
  , links = IntMap.fromList [ (coerce c, oddNode) | c <- alphabet @n ]
  }
