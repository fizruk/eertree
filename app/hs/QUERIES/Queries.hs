-- add or pop characters from a string, at each step get the 
-- number of palindromes in the string

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where
import EERTREE.Simple (EERTREE, popBackTreeVersion, append, empty, frequency)
import EERTREE.Node
import           GHC.TypeLits    (KnownNat)
import           EERTREE.Symbol
import Data.Char

getNumberOfPalindromes :: KnownNat n => EERTREE n -> Int 
getNumberOfPalindromes tree = sum $ map snd (frequency tree)

processQueries :: KnownNat n => Int -> EERTREE n -> IO ()
processQueries 0 _ = return ()
processQueries x tree = do
    char <- getChar
    let newTree = case char of
            '-' -> popBackTreeVersion tree
            c   -> append (Symbol (lowercaseEnglishToSymbol c)) tree
    putStr(show (getNumberOfPalindromes newTree) ++ " ")
    processQueries (x - 1) newTree

main :: IO ()
main = do 
    t <- readLn @Int
    processQueries t (empty @26)
    putStrLn ""
