-- agda-ghc-names.hs
-- Copyright 2015 by Wolfram Kahl
-- Licensed under the same terms as Agda

module Main (main) where

import ResolveHsNames (getResolveHsNamesMap, writeResolveHsNamesMap, apply2M, splitMapApply)
import FixProf (updateProfFile)
import FixSource (updateSourceFile)
import Find (find)

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import Debug.Trace

printUsage :: String -> IO ()
printUsage cmd = hPutStrLn stderr $ "Usage: agda-ghc-names " ++ cmd

main :: IO ()
main = do
  args0 <- getArgs
  case args0 of
    "extract"  : args -> extract  args
    "fixprof"  : args -> fixprof  args
    "fixsrc"   : args -> fixsrc   args
    "find"     : args -> find     printUsage args
    _ -> printUsage "(fixprof|fixsrc|extract|find) {command args.}"

extract :: [String] -> IO ()
extract args = case args of
    [dir] -> do
      (outFile, _) <- writeResolveHsNamesMap dir
      hPutStrLn stderr $ "wrote " ++ outFile
    _ -> printUsage "extract <directory>"

fixprof :: [String] -> IO ()
fixprof = uncurry fixProf' . getOpts False False
  where
    getOpts m s ("+m" : args) = getOpts True s args
    getOpts m s ("+s" : args) = getOpts m True args
    getOpts m s args = ((m, s), args)

fixProf' (keepMod, keepSrc) args = case args of
    [] -> usage
    [_] -> usage
    dir : profs -> do
      resolve <- liftM apply2M $ getResolveHsNamesMap dir
      mapM_ (updateProfFile usage resolve keepMod keepSrc) profs
  where usage = printUsage "fixprof {+m} {+s} <dir> <File>.prof"

fixsrc :: [String] -> IO ()
fixsrc = uncurry fixSource' . getOpts False False
  where
    getOpts m s ("+m" : args) = getOpts True s args
    getOpts m s ("+s" : args) = getOpts m True args
    getOpts m s args = ((m, s), args)

fixSource' (keepMod, keepSrc) args = case args of
    [] -> usage
    [_] -> usage
    dir : sources -> do
      resolve <- liftM apply2M $ getResolveHsNamesMap dir
      mapM_ (updateSourceFile usage resolve keepMod keepSrc) sources
  where usage = printUsage "fixsrc {+m} {+s} <dir> <File>.hs"
