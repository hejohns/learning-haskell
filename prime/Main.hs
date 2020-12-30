{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main (main) where

{-@ reflect noZeros @-}
noZeros :: Integral a => [a] -> Bool
noZeros [] = True
noZeros l@(hd:tl) = hd /= 0 && noZeros tl

{-@ prime :: (Integral a, Eq b, Num b) => {v : [a] | noZeros v} -> b -> b -> [a] @-}
prime :: (Integral a, Eq b, Num b) => [a] -> b -> b -> [a]
prime [] b c = []
prime a@(h:t) b c
    | b == c = a
    | otherwise = h:(prime (filter (\x -> (x `mod` h) /= 0) t) (b + 1) c)

main :: IO ()
main = putStrLn $ show $ prime [2..] 0 1000
