module Main (main) where

import Control.Parallel

{-@ type Natural = {v : Integer | v >= 0} @-}
{-@ fib :: Natural -> Natural @-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
    k `par` (l `pseq` m)
    where
    k = show $ fib 40
    l = show $ length [1..1000000000]
    m = putStrLn (k ++ l)
