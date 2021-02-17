{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-@ LIQUID "--reflection" @-}
module Main (main) where
import Control.Monad.ST
import Data.STRef

{-@ ignore main @-}
main :: IO ()
main = do
    putStrLn ret
    where
    ret = runST (do
        a <- newSTRef "This is a string"
        writeSTRef a "A different string"
        aVal <- readSTRef a
        case aVal of
            "A different string" -> writeSTRef a "Boom"
            _ -> writeSTRef a "Nope"
        aVal <- readSTRef a
        return aVal
        )
