module Main (main) where

import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- Import day modules here as they are created
-- import qualified Day01

main :: IO ()
main = do
  putStrLn "🎄 Advent of Code - Haskell Solutions"
  putStrLn "======================================"
  
  start <- getCurrentTime
  
  -- Run solutions here as they are created
  -- runDay 1 Day01.main
  
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  printf "\nTotal time: %.2fs\n" (realToFrac elapsed :: Double)

runDay :: Int -> IO () -> IO ()
runDay n action = do
  printf "\n----------\n| Day %02d |\n----------\n" n
  action
