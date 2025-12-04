module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callProcess)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> runDayTests (read dayStr)
    _ -> die "Usage: cabal run test <day>"

runDayTests :: Int -> IO ()
runDayTests day = do
  let dayPadded = printf "%02d" day :: String
      pattern = printf "/Day%s/" dayPadded
      opt = "--test-options=--match " ++ pattern
  -- Run `cabal test --test-options=--match /DayNN/`
  callProcess "cabal" ["test", opt]
