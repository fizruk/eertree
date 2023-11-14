-- add or pop characters from a string, at each step get the
-- number of palindromes in the string

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State
import qualified Data.Map as Map
import EERTREE.List
import EERTREE.Node
import GHC.TypeLits    (KnownNat)
import EERTREE.Symbol

-- getNumberOfPalindromes :: KnownNat n => EERTREE n -> Int
-- getNumberOfPalindromes tree = sum $ map snd (frequency tree)

-- processQueries :: KnownNat n => Int -> EERTREE n -> IO ()
-- processQueries 0 _ = return ()
-- processQueries x tree = do
--     char <- getChar
--     let newTree = case char of
--             '-' -> popBackTreeVersion tree
--             c   -> append (Symbol (lowercaseEnglishToSymbol c)) tree
--     putStr(show (getNumberOfPalindromes newTree) ++ " ")
--     processQueries (x - 1) newTree

palindromicSuffixes :: KnownNat n => Node n -> [Node n]
palindromicSuffixes node
    | len node > 0 = node : palindromicSuffixes (link node)
    | otherwise = []

main :: IO ()
main = do
    _ <- getLine
    queries <- getLine
    _ <- flip execStateT [(empty @26, 0)] $
        forM_ queries $ \case
            '-' -> do
                -- { state = [(t3, n3), (t2, n2), (t1, n1)] }
                modify tail                 -- state' := tail state
                -- { state = [(t2, n2), (t1, n1)] }
                (_, n) : _ <- get
                -- { state = [(t2, n2), (t1, n1)], n = n2 }
                liftIO $ putStr (show n)
            c -> do
                -- { state = [(t3, n3), (t2, n2), (t1, n1)] }
                (t, n) : _ <- get
                -- { t = t3, n = n3 }
                let s = Symbol (lowercaseEnglishToSymbol c)
                let t' = prepend s t
                let !n' = n + length (palindromicSuffixes (maxSuffix t'))
                modify ((t', n') : )          -- state' := (t', n') : state
    putStrLn ""
