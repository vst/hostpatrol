module Main where

import qualified HostPatrol.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.cli >>= exitWith
