{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Void

data Union a b where
    One :: a -> Union a Void
    Two :: b -> Union Void b
    Both :: (Eq a, Eq b, Show a, Show b) => a -> b -> Union a b

deriving instance (Eq a, Eq b) => Eq (Union a b)
deriving instance (Show a, Show b) => Show (Union a b)

x = One 1
y = Two 2
z = Both ("test" :: T.Text) 3

main :: IO ()
main =
    return ()
