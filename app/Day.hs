module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> do
      let day = read dayStr :: Int
      runDay day
    _ -> do
      putStrLn "Usage: cabal run day <day>"
      exitFailure

runDay :: Int -> IO ()
runDay day = do
  printf "Day %02d not yet implemented\n" day
  exitFailure
