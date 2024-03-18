-- add or pop characters from a string, at each step get the
-- number of palindromes in the string
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import EERTREE.ApplicationsDE (updatePalSuffixesCount)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import EERTREE.List
import EERTREE.Node
import GHC.TypeLits    (KnownNat)
import EERTREE.Symbol

-- For interactive queries
main :: IO ()
main = do
    _ <- getLine
    queries <- getLine
    _ <- flip execStateT ([(empty @26, 0)], Map.empty) $
        forM_ queries $ \case
            '-' -> do
                -- { state = [(t3, n3), (t2, n2), (t1, n1)] }
                modify (\(x, y) -> (tail x, y))                 -- state' := tail state
                -- { state = [(t2, n2), (t1, n1)] }
                (_, n) : _ <- gets fst
                -- { state = [(t2, n2), (t1, n1)], n = n2 }
                liftIO $ putStr (show n ++ " ")
            c -> do
                -- { state = [(t3, n3), (t2, n2), (t1, n1)] }
                (t, n) : _ <- gets fst
                -- { t = t3, n = n3 }
                let s = Symbol (lowercaseEnglishToSymbol c)
                let t' = prepend s t
                d <- updatePalSuffixesCount (maxPrefix t')
                let !n' = n + d
                modify (\(x, y) -> ((t', n') : x, y))          -- state' := (t', n') : state
                liftIO $ putStr (show n' ++ " ")
    putStrLn ""
