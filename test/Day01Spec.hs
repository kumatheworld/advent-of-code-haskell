module Day01Spec (spec) where

import Test.Hspec
import qualified Day01
import AoC.Template (readExample, Day(..))

spec :: Spec
spec = describe "Day01" $ do
  it "solves part 1 correctly" $ do
    input <- readExample (Day 1)
    Day01.part1 input `shouldBe` Just 15
  
  it "solves part 2 correctly" $ do
    input <- readExample (Day 1)
    Day01.part2 input `shouldBe` Just 120
