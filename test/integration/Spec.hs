import Test.Hspec

main :: IO ()
main = hspec $
  describe "Test the expected input / output of the program" $
    it "Compares the input and the output" $
      "stub" `shouldBe` "stub"
