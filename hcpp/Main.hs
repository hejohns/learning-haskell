{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-@ LIQUID "--reflection" @-}
module Main (main) where
import System.Environment
import System.Console.GetOpt
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

data ConsoleOptions where
    Help :: ConsoleOptions
    Version :: ConsoleOptions
    deriving (Eq, Show)

{-@ ignore main @-}
main :: IO ()
main = do
    argv <- getArgs
    let (opts :: [ConsoleOptions], nonOpts :: [String], errors :: [String]) = (getOpt__ argv)
    let processOpts__ = opts
    -- yes, I know this fails
    case head opts of
        Help -> putStrLn helpMesg__
        Version -> putStrLn versionMesg__
    if length nonOpts == 0
    then
        -- stdin
        do
        let dict = (Map.empty :: Map.Map String String)
        let dict' = Map.insert "a" "b" dict
        putStrLn $ show $ dict'
    else
        putStrLn $ show $ head nonOpts
    where
    getOpt__ = getOpt
        Permute
        [
            (Option ['h'] ["help"] (NoArg Help) helpMesg__),
            (Option ['v'] ["version"] (NoArg Version) versionMesg__)
        ]
    helpMesg__ = "Usage: hcpp [-h] [-v]"
    versionMesg__ = "hcpp 0.0\nWritten by Some Dummy"
    --processOpts__ Help = a
