{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
module Main (main) where

#if 0
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
#endif

{-@ ignore take' @-}
take' :: (Eq a, Num a) => a -> [b] -> [b]
take' 0 _ = []
take' _ [] = []
take' n l@(hd:tl) = hd:(take' (n - 1) tl)

{-@ ignore prime @-}
prime :: (Integral a, Eq b, Num b) => [a] -> b -> b -> [a]
prime [] s u = []
prime l@(h:t) s u
    | s == u = []
    | otherwise = h:(prime (filter (\x -> (x `mod` h) /= 0) t) (s + 1) u)

main :: IO ()
main = putStrLn $ show $ prime p 0 10
    where
    p = [2..10000]
