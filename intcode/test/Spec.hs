import Test.Hspec
import Intcode

main :: IO ()
main = hspec $ do
  describe "run" $
    it "runs the provided intcode program" $ do
      run "1,0,0,0,99" `shouldBe` Right "2,0,0,0,99"
      run "2,3,0,3,99" `shouldBe` Right "2,3,0,6,99"
      run "2,4,4,5,99,0" `shouldBe` Right "2,4,4,5,99,9801"
      run "1,1,1,4,99,5,6,0,99" `shouldBe` Right "30,1,1,4,2,5,6,0,99"
      run "1,5,0,0,99" `shouldBe` Left "Invalid address"
      run "1,0,5,0,99" `shouldBe` Left "Invalid address"
      run "1,0,0,5,99" `shouldBe` Left "Invalid address"
      run "1" `shouldBe` Left "Invalid address"
      run "1,0" `shouldBe` Left "Invalid address"
      run "1,0,0" `shouldBe` Left "Invalid address"
      run "1,0,0,0" `shouldBe` Left "Invalid address"
      run "1002,4,3,4,33" `shouldBe` Right "1002,4,3,4,99"

  describe "runWithParams" $
    it "replaces the two parameters at addresses 1 and 2, runs the program, returns position 0" $
      runWithParams "1,0,0,4,99,5,6,0,99" 1 1 `shouldBe` Right 30

  describe "runWithIO" $
    it "receives inputs and returns outputs" $
      runWithIO ("3,5,4,5,99,0", [42])  `shouldBe` Right ("3,5,4,5,99,42", [42])
