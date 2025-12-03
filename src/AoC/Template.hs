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

readExample :: Day -> IO T.Text
readExample (Day n) = TIO.readFile $ printf "data/examples/%02d.txt" n

solve :: (Show a) => Day -> (T.Text -> Maybe a) -> (T.Text -> Maybe a) -> IO ()
solve day part1 part2 = do
  input <- readInput day
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  hFlush stdout
