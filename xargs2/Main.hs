{-@ LIQUID "--reflection" @-}
module Main (main) where

import Data.List
import Data.Traversable
import Data.Foldable
import Language.Haskell.Liquid.Prelude (unsafeError, liquidError)

import System.Environment
import System.Process
import System.IO
import System.Exit

{-
nonEmptyString :: String -> Maybe NonEmptyString
nonEmptyString str
    | length str > 0 = Just $ NonEmptyString str
    | otherwise = Nothing

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
    --if length argv
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
-}

{-@ type NonEmptyString = NonEmptyList Char @-}
{-@ type NonEmptyList a = {v : [a] | llen v > 0} @-}

{-@ measure llen @-}
{-@ llen :: [a] -> {v : Int | v >= 0} @-}
llen :: [a] -> Int
llen [] = 0
llen (hd:tl) = 1 + llen tl

{-@ impossible :: {v : String | false} -> a @-}
impossible msg = error msg

{-@ substitute :: String -> NonEmptyString -> String -> String @-}
substitute :: String -> String -> String -> String
substitute str match r = str

{-@ nonEmptyString :: String -> Maybe NonEmptyString @-}
nonEmptyString :: String -> Maybe String
nonEmptyString "" = Nothing
nonEmptyString str@(hd:tl) = Just str

{-@ nonEmptyList :: [a] -> Maybe (NonEmptyList a) @-}
nonEmptyList :: [a] -> Maybe [a]
nonEmptyList [] = Nothing
nonEmptyList l@(hd:tl) = Just l

main :: IO()
main = undefined
