{-# LANGUAGE GADTs, ScopedTypeVariables, ForeignFunctionInterface #-}
{-@ LIQUID "--reflection" @-}
module Main (main) where
import System.Environment
import System.Console.GetOpt
import System.IO
import qualified Data.Map.Lazy as Map
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Data.Char
import qualified GHC.IO

{-
  #define FAKE DEF
-}

data ConsoleOptions where
    Help :: ConsoleOptions
    Version :: ConsoleOptions
    deriving (Eq, Show)

{-@ ignore normal @-}
normal :: Handle -> Map.Map String String -> IO ()
normal han dict = do
    join $ stToIO (do
        dictRef <- newSTRef Map.empty
        dictVal <- readSTRef dictRef
        let fun :: IO () = do
                done <- hIsEOF han
                case done of
                    True ->
                        return ()
                    False -> do
                        line <- hGetLine han
                        case dropSpaces line of
                            '#':'d':'e':'f':'i':'n':'e':tl ->
                                putStrLn $ fstWord $ dropSpaces tl
                            hd:tl ->
                                case dictVal Map.!? (fstWord (hd:tl)) of
                                Just body ->
                                    putStrLn body
                                Nothing ->
                                    putStrLn line
                            _ ->
                                putStrLn line
                        stToIO $ modifySTRef dictRef (Map.insert "key" line)
                        dictVal <- stToIO $ readSTRef dictRef
                        fun
        GHC.IO.ioToST fun
        {-
        case dictVal Map.!? "key" of
            Just x -> return $ putStrLn x
            Nothing -> return $ putStrLn "fail"
        -}
        return $ return ()
        )
    where
    cont = normal han dict
    dropSpaces = dropWhile isSpace
    fstWord = takeWhile isAlphaNum

main :: IO ()
main = do
    hSetEncoding stdin utf8
    argv <- getArgs
    let (opts :: [ConsoleOptions], nonOpts :: [String], errors :: [String]) = getOpt__ argv
    exitEarly <- runMaybeT (do
        case errors of
            hd:tl -> do
                liftIO $ hPutStrLn stderr hd
                mzero
            _ ->
                return ()
        case opts of
            Help:tl -> do
                liftIO $ hPutStrLn stderr helpMesg__
                mzero
            Version:tl -> do
                liftIO $ hPutStrLn stderr versionMesg__
                mzero
            _ ->
                return ()
        case nonOpts of
            hd:tl -> do
            -- read from file
                fh <- liftIO $ openFile hd ReadMode
                return fh
            _ ->
            -- read from stdin
                return stdin
        )
    case exitEarly of
        Nothing -> 
            -- exit early
            return ()
        Just fh -> do
            normal fh Map.empty
    where
    getOpt__ = getOpt
        Permute
        [
            Option ['h'] ["help"] (NoArg Help) helpMesg__,
            Option ['v'] ["version"] (NoArg Version) versionMesg__
        ]
    helpMesg__ = "Usage: hcpp [-h] [-v]"
    versionMesg__ = "hcpp 0.0\nWritten by Some Dummy"
    l :: Map.Map String String -> (Int, Map.Map String String)
    l s = (0, Map.insert "a" "b" s)
    k = Map.empty :: Map.Map String String
