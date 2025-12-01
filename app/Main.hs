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
  -- Day01.solution
  
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  printf "\nTotal time: %.2fs\n" (realToFrac elapsed :: Double)
