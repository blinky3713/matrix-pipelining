module Main where

import           Build_doctests     (flags, module_sources, pkgs)
import           Prelude
import           System.Environment (lookupEnv)
import           System.Process
import           Test.DocTest       (doctest)

getGlobalPackageDb :: IO String
getGlobalPackageDb = readProcess "ghc" ["--print-global-package-db"] ""

main :: IO ()
main = do
  inNixShell <-lookupEnv "IN_NIX_SHELL"
  extraFlags <-
    case inNixShell of
      Nothing -> pure []
      Just _  -> pure . ("-package-db="++) <$> getGlobalPackageDb

  doctest (flags ++ extraFlags ++ pkgs ++ module_sources)
