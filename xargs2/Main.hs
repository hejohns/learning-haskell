module Main (main) where

import System.Environment
import System.Process
import System.IO
import System.Exit
import Data.List
import Data.Traversable
import Data.Foldable

newtype NonEmptyString = NonEmptyString String

nonEmptyString :: String -> Maybe NonEmptyString
nonEmptyString str = if length str > 0
    then Just $ NonEmptyString str
    else Nothing

{- Data.Text replace, just wanted to write it myself -}
substitute :: String -> NonEmptyString -> String -> String
substitute "" _ _ = ""
substitute str@(hd:tl) match@(NonEmptyString match') r =
    case length match' <= length str of
        True -> case isPrefixOf match' str of
            True -> r ++ substitute (drop (length match') str) match r
            False -> hd:substitute tl match r
        False -> str

myFold f [] = return ()
myFold f a@(hd:tl) = f hd >> myFold f tl

main :: IO ()
main = do
    argv <- getArgs
    let maybeDelim = stripPrefix "-I" (head argv)
    case (2 <= length argv, maybeDelim) of
        (True, Just delim) ->
            case nonEmptyString delim of
                Just delim ->
                    let a = substitute (concat (intersperse " " (tail argv))) delim in
                    let b = repeat (isEOF >>= (\x -> if x then exitWith ExitSuccess else getLine)) in
                    let c = myFold (>>= callCommand . a) b in
                    --let c = traverse_ (>>= callCommand . a) b in
                    --putStrLn $ substitute "Hello, Haskell!" (NonEmptyString "Hell") "G"
                    --do
                    --callCommand (a "ABC")
                    c
                _ -> help
        (_, _) -> help
    where
    help = putStrLn "Usage: xargs -I{} string"
