module Main (main) where

import System.Environment
import System.Process
import System.IO
import System.Exit
import Data.List
import Data.Traversable
import Data.Foldable

newtype NonemptyString = NonemptyString String

{- Data.Text replace, just wanted to write it myself -}
substitute :: String -> NonemptyString -> String -> String
substitute "" (NonemptyString match) r = ""
substitute str@(hd:tl) (NonemptyString match) r =
    case length match <= length str of
        True -> case isPrefixOf match str of
            True -> r ++ substitute (drop (length match) str) (NonemptyString match) r
            False -> hd:substitute tl (NonemptyString match) r
        False -> str


main :: IO ()
main = do
    argv <- getArgs
    let maybeDelim = stripPrefix "-I" (head argv)
    if 2 <= length argv && maybeDelim /= Nothing
        then
            let Just __delim = maybeDelim in
            if length __delim > 0
            then
                let delim = NonemptyString __delim in
                    let a = substitute (concat (intersperse " " (tail argv))) delim in
                    let b = repeat (isEOF >>= (\x -> if x then exitWith ExitSuccess else getLine)) in
                    let c = traverse_ (>>= callCommand . a) b in
                    --putStrLn $ substitute "Hello, Haskell!" (NonemptyString "Hell") "G"
                    --do
                    --callCommand (a "ABC")
                    c
            else
                help
        else help
    where
    help = putStrLn "Usage: xargs -I{} string"
