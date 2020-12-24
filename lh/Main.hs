module Main (main) where

import Language.Haskell.Liquid.Prelude
import Data.List

{-@
    data List a = Nil | (:::) {hd :: a, tl :: List a}
@-}
data List a = Nil | (:::) a (List a)

{-@ measure llength @-}
{-@ llength :: List a -> {v : Integer | v >= 0} @-}
llength :: List a -> Integer
llength Nil = 0
llength (hd:::tl) = 1 + llength tl

{-@ ltail :: {v : List a | llength v > 0} -> List a @-}
ltail :: List a -> List a
ltail Nil = Nil
ltail (hd:::tl) = tl

{-@ type LNonEmpty a = {v : List a | llength v > 0} @-}
{-@ lnonEmpty :: List a -> Maybe (LNonEmpty a)@-}
lnonEmpty :: List a -> Maybe (List a)
lnonEmpty Nil = Nothing
--lnonEmpty Nil = Just Nil
lnonEmpty l@(hd:::tl) = Just l

main :: IO ()
main = (ltail (1:::(2:::(3:::Nil)))) `seq` putStrLn "Hello, Haskell!"
--main = (ltail Nil) `seq` putStrLn "Hello, Haskell!"
