{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main (main) where

{-@ reflect noZeros @-}
{-@ noZeros :: (Eq a, Num a) => [a] -> Bool @-}
noZeros :: (Eq a, Num a) => [a] -> Bool
noZeros [] = True
noZeros l@(hd:tl) = hd /= (0 :: Num a => a) && noZeros tl

{-@ prime :: (Integral a, Eq b, Num b) => {v : [a] | noZeros v} -> b -> b -> [a] @-}
prime :: (Integral a, Eq b, Num b) => [a] -> b -> b -> [a]
prime [] b c = []
prime a@(h:t) b c
    | b == c = a
    | otherwise = h:(prime (filter (\x -> (x `mod` h) /= 0) t) (b + 1) c)

main :: IO ()
main = putStrLn $ show $ prime p 0 1000
    where
{-@ assume p :: {v : [Integer] | noZeros v} @-}
    p = [2..10000]
