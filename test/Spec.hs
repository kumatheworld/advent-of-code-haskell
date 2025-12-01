import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Advent of Code Tests" $ do
    it "placeholder test" $ do
      True `shouldBe` True
