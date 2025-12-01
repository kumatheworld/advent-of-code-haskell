module AoC.Template
  ( readInput
  , readExample
  , solve
  , solveAndSubmit
  , Day(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import System.Process (callCommand)
import Text.Printf (printf)

newtype Day = Day Int

readInput :: Day -> IO T.Text
readInput (Day n) = TIO.readFile $ printf "data/inputs/%02d.txt" n

readExample :: Day -> IO T.Text
readExample (Day n) = TIO.readFile $ printf "data/examples/%02d.txt" n

solve :: Show a => Day -> (T.Text -> Maybe a) -> (T.Text -> Maybe a) -> IO ()
solve day part1 part2 = do
  input <- readInput day
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  hFlush stdout

solveAndSubmit :: Show a => Day -> (T.Text -> Maybe a) -> (T.Text -> Maybe a) -> IO ()
solveAndSubmit day@(Day n) part1 part2 = do
  args <- getArgs
  input <- readInput day
  
  let answer1 = part1 input
      answer2 = part2 input
  
  putStrLn $ "Part 1: " ++ show answer1
  putStrLn $ "Part 2: " ++ show answer2
  hFlush stdout
  
  when ("--submit" `elem` args) $ do
    case answer1 of
      Just a1 -> do
        putStrLn "\nSubmitting Part 1..."
        callCommand $ printf "cabal run submit %d 1 %s" n (show a1)
      Nothing -> putStrLn "Part 1: No answer to submit"
    
    case answer2 of
      Just a2 -> do
        putStrLn "\nSubmitting Part 2..."
        callCommand $ printf "cabal run submit %d 2 %s" n (show a2)
      Nothing -> putStrLn "Part 2: No answer to submit"

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()
