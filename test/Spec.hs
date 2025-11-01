import Test.Hspec

-- Import day test modules here as they are created
import qualified Day01Spec

main :: IO ()
main = hspec $ do
  describe "Advent of Code Tests" $ do
    it "placeholder test" $ do
      True `shouldBe` True
    
    -- Add day test suites here as they are created
    Day01Spec.spec
