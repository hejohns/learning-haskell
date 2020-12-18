module Main (main) where

import System.Environment
import Data.List
import Data.Traversable

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

--traverse (>>= func ) (repeat getLine)

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
                    let d = substitute (concat (tail argv)) delim in
                    --putStrLn $ substitute "Hello, Haskell!" (NonemptyString "Hell") "G"
                    putStrLn $ d "ABC"
            else
                help
        else help
    where
    help = putStrLn "Usage: xargs -I{} string"
