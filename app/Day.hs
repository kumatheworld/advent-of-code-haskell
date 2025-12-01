module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callCommand)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> runDay (read dayStr)
    _ -> die "Usage: cabal run day <day>"

runDay :: Int -> IO ()
runDay day = do
  let dayPadded = printf "%02d" day :: String
  callCommand $ printf "cabal run day%s" dayPadded
