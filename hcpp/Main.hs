{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-@ LIQUID "--reflection" @-}
module Main (main) where
import System.Environment
import System.Console.GetOpt
import System.IO
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

data ConsoleOptions where
    Help :: ConsoleOptions
    Version :: ConsoleOptions
    deriving (Eq, Show)

{-@ ignore normal @-}
normal :: Map.Map String String -> IO (Map.Map String String)
normal dict = do
    done <- isEOF
    if done
        then return dict
        else do
            line <- getLine
            normal (Map.insert "a" line dict)

test a = do
    if True
    then
        do
            b <- getLine
            return b
    else
        return "String"

{-@ ignore main @-}
main :: IO ()
main = do
    argv <- getArgs
    let (opts :: [ConsoleOptions], nonOpts :: [String], errors :: [String]) = getOpt__ argv
    let processOpts__ = opts
    -- yes, I know this fails
    if null opts
    then
        putStrLn ""
    else
        case head opts of
            Help -> putStrLn helpMesg__
            Version -> putStrLn versionMesg__
    if null nonOpts
    then
        -- stdin
        --putStrLn $ show $ execState (state l) k
        do
            m <- normal Map.empty
            case m Map.!? "a" of
                Just m' -> putStrLn $ m'
                Nothing -> return ()
    else
        putStrLn $ show $ head nonOpts
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
    --processOpts__ Help = a
