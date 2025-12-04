module AoC.Template
  ( readInput,
    readExample,
    solve,
    Day (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Text.Printf (printf)

newtype Day = Day Int

readInput :: Day -> IO T.Text
readInput (Day n) = TIO.readFile $ printf "data/inputs/%02d.txt" n

-- | Read a numbered example file (data/examples/DD-N.txt)
-- Example: readExample (Day 1) 1 reads "data/examples/01-1.txt"
--          readExample (Day 1) 2 reads "data/examples/01-2.txt"
readExample :: Day -> Int -> IO T.Text
readExample (Day n) exampleNum = TIO.readFile $ printf "data/examples/%02d-%d.txt" n exampleNum

solve :: (Show a) => Day -> (T.Text -> Maybe a) -> (T.Text -> Maybe a) -> IO ()
solve day part1 part2 = do
  input <- readInput day
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  hFlush stdout
