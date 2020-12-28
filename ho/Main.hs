{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main (main) where

map__ :: forall a.Ord a => (forall b.Ord b => b -> b -> b) -> [a] -> [a]
map__ f [] = []
map__ f l@(hd:tl) = map__' f l hd

map__' :: forall a.Ord a => (forall b.Ord b => b -> b -> b) -> [a] -> a -> [a]
map__' f [] sen = []
map__' f l@(hd:tl) sen =
    --big:(map__' f l big)
    big:(map__' f tl big)
    where big = f hd sen

main :: IO ()
main = putStrLn $ show $ map__ f [1, 2, 3, 6, 8, 11, 4, 5, 6]
    where
    f :: Ord a => a -> a -> a
    f l r = max l r
