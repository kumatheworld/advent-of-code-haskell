module Day01 (solution, part1, part2) where

import AoC.Template (Day(..), solve)
import qualified Data.Text as T

day :: Day
day = Day 1

part1 :: T.Text -> Maybe Int
part1 input = Just $ sum $ map (read . T.unpack) $ T.lines input

part2 :: T.Text -> Maybe Int
part2 input = Just $ product $ map (read . T.unpack) $ T.lines input

solution :: IO ()
solution = solve day part1 part2
