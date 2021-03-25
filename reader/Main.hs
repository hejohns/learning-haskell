#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where
import Data.IORef

main = do
  intRef <- newIORef (0 :: Integer)
  runReader (do
      e <- ask
      return (do
        modifyIORef e (+ 1)
        putStrLn "look at this IORef!"
        int <- readIORef e
        putStrLn . show $ int
        modifyIORef e (+ 1)
        )
      --return $ readIORef e
      ) (intRef)
  int <- readIORef intRef
  putStrLn . show $ int

newtype Reader env r = Reader {runReader :: env -> r}

instance Functor (Reader env) where
  f `fmap` Reader a = Reader $ \e -> f (a e)

instance Applicative (Reader env) where
  pure a = Reader $ \_ -> a
  Reader f <*> Reader a = Reader $ \e -> f e (a e)

instance Monad (Reader env) where
  Reader a >>= f = Reader $ \e -> runReader (f (a e)) e
  return a = Reader (\_ -> a)

ask :: Reader env env
ask = Reader id
