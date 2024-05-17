{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications    #-}

module EERTREE.Applications.Unidirectional where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import EERTREE.Tree.Unidirectional
import EERTREE.Node
import GHC.TypeLits    (KnownNat)
import           Data.Ord                    (comparing)
import           Data.List                   (maximumBy)
import Data.Foldable as F
import EERTREE.Symbol
import Control.Parallel.Strategies
import GHC.Conc (numCapabilities)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

-- | Online updating of palindromic suffixes
palindromicSuffixes :: KnownNat n => Node n -> [Node n]
palindromicSuffixes node
    | len node > 0 = node : palindromicSuffixes (link node)
    | otherwise = []

updatePalSuffixesCount :: (MonadState ([(EERTREE n, Int)], Map Integer Int) m, KnownNat n) => Node n -> m Int
updatePalSuffixesCount node
    | len node <= 0 = return 0
    | otherwise = do
        (_, tab) <- get
        let i = index node
        case Map.lookup i tab of
            Nothing -> do
                k <- updatePalSuffixesCount (link node)
                let !n = k + 1
                modify (\(x, tab) -> (x, Map.insert i n tab))
                return n
            Just n -> return n

-- | State for Queries of insertion and deletion
type StateType = ([(EERTREE 26, Int)], Map Integer Int)

processQuery :: Char -> State StateType ()
processQuery '-' = do
    -- Remove the most recent entry
    modify (\(x, y) -> (tail x, y))
    xs <- gets fst
    case xs of
        [] -> return ()
        (_, !n) : _ -> return ()  

processQuery c = do
    xs <- gets fst
    case xs of 
        [] -> return ()
        (t, !n) : _ -> do
            let s = Symbol (lowercaseEnglishToSymbol c)
            let t' = prepend s t
            d <- updatePalSuffixesCount (maxPrefix t')
            let !n' = n + d
            modify (\(x, y) -> ((t', n') : x, y))
            return()

-- For bench
getQueries :: String -> [Int]
getQueries queries = 
    let initialState = ([(empty @26, 0)], Map.empty)
        finalState = execState (mapM_ processQuery queries) initialState
        results = map snd $ Map.toList (snd finalState) 
    in results

------------------------------------------

countMax :: Ord a => a -> [a] -> (a, Int)
countMax z = foldl' step (z, 1)
  where
    step acc@(x, n) y
      | y > x = (y, 1)
      | y == x = (x, n + 1)
      | otherwise = acc

countMax' :: Ord a => [a] -> (a, Int)
countMax' [] = error "empty list"
countMax' (x:xs) = countMax x xs

-- For bench
getMsubstr :: String -> (Int, Int)
getMsubstr str = countMax' (map len (palindromes tree))
  where
    tree = eertreeFromString @26 str

-- getMsubstrPar :: Text -> (Int, Int)
-- getMsubstrPar input = countMax' (map len (palindromes tree))
--   where
--     cores = numCapabilities
--     chunkSize = Text.length input `div` fromIntegral cores
--     tree = foldr merge empty chunks
--     chunks =
--       [ eertreeFromString @26 (Text.unpack chunk)
--       | chunk <- Text.chunksOf chunkSize input
--       ] `using` parList rpar
