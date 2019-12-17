import Test.Hspec
import Intcode

main :: IO ()
main = hspec $ do
  describe "run" $
    it "receives inputs and returns outputs" $ do
      run ("1,0,0,0,99", []) `shouldBe` Right ("2,0,0,0,99", [])
      run ("2,3,0,3,99", []) `shouldBe` Right ("2,3,0,6,99", [])
      run ("2,4,4,5,99,0", []) `shouldBe` Right ("2,4,4,5,99,9801", [])
      run ("1,1,1,4,99,5,6,0,99", []) `shouldBe` Right ("30,1,1,4,2,5,6,0,99", [])
      run ("1,5,0,0,99", []) `shouldBe` Left "Invalid address"
      run ("1,0,5,0,99", []) `shouldBe` Left "Invalid address"
      run ("1,0,0,5,99", []) `shouldBe` Left "Invalid address"
      run ("1", []) `shouldBe` Left "Invalid address"
      run ("1,0", []) `shouldBe` Left "Invalid address"
      run ("1,0,0", []) `shouldBe` Left "Invalid address"
      run ("1,0,0,0", []) `shouldBe` Left "Invalid address"
      run ("1002,4,3,4,33", []) `shouldBe` Right ("1002,4,3,4,99", [])
      run ("3,5,4,5,99,0", [42])  `shouldBe` Right ("3,5,4,5,99,42", [42])
      snd <$> run ("3,9,8,9,10,9,4,9,99,-1,8", [8]) `shouldBe` Right [1]
      snd <$> run ("3,9,8,9,10,9,4,9,99,-1,8", [7]) `shouldBe` Right [0]
