{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ejercicio2 (wordCount) where
import Data.Char(toLower,isAlphaNum,isSpace)
import Control.Monad(liftM2)
import Data.List(nub)
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char ( toUpper, toLower )
import Text.Read (Lexeme(String))

getWords :: String  -> [String]
getWords xs = map tidyWords $ words $ filter noPuncBQ $ commasToSpaces xs
    where   commasToSpaces = map (\x -> if x == ',' then ' ' else x)
            tidyWords = removeQuotes . map toLower
            noPuncBQ c = isAlphaNum c || isSpace c || c =='\''
removeQuotes :: String -> String
removeQuotes [] = []
removeQuotes [a] = [a]
removeQuotes xs = if (head xs == last xs) && ( (head xs == '\'') || (head xs == '\"') ) then (init . tail) xs else xs

countWord :: String -> [String] -> Int
countWord = (length .) . filter . (==)

wordCount :: String -> [(String, Int)]
wordCount xs = nub $ map (\x -> (x, countWord x wordlist) ) wordlist
                where wordlist = getWords xs


mayusculaInicial :: String -> String
mayusculaInicial = unwords . map capitalize . words
    where capitalize (x:xs) = toUpper x : map toLower xs