{-# LANGUAGE GADTs, ScopedTypeVariables #-}
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
        let fun :: IO () = do
                done <- hIsEOF han
                case done of
                    True ->
                        return ()
                    False -> do
                        line <- hGetLine han
                        case dropSpaces line of
                            '#':'d':'e':'f':'i':'n':'e':tl ->
                                putStrLn tl
                            _ ->
                                putStrLn line
                        stToIO $ modifySTRef dictRef (Map.insert "key" line)
                        fun
        GHC.IO.ioToST fun
        dictVal <- readSTRef dictRef
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
    {-
    dirty = let line1 = dropSpaces line in
            let line2 = stripPrefix "#" line1 in
            case line2 of
                Nothing ->
                    cont
                Just a ->
                    let line3 = dropSpaces a in
                    let n = fstWord in
                    let b = 
                False ->
                    normal han (Map.insert "a" line dict)
    -}

{-@ ignore main @-}
main :: IO ()
main = do
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
                MaybeT $ return Nothing
            Version:tl -> do
                liftIO $ hPutStrLn stderr versionMesg__
                mzero
            _ ->
                return ()
        case nonOpts of
            hd:tl ->
            -- read from file
                return ()
            _ ->
            -- read from stdin
                return ()
        )
    case exitEarly of
        Nothing -> 
            -- exit early
            return ()
        Just a -> do
            normal stdin Map.empty
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
