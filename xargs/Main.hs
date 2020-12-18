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
            True -> r ++ drop (length match) str
            False -> hd:substitute tl (NonemptyString match) r
        False -> str

--traverse (>>= func ) (repeat getLine)

main :: IO ()
main = do
    putStrLn $ substitute "Hello, Haskell!" (NonemptyString "Hell") "G"
