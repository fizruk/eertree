{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications    #-}

module EERTREE.ApplicationsDE where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import EERTREE.List
import EERTREE.Node
import GHC.TypeLits    (KnownNat)
import EERTREE.Symbol

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
