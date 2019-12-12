import Test.Hspec
import Lib

main :: IO ()
main = hspec $
  describe "runIntcode" $
    it "runs Intcode correctly" $ do
      runIntcode "1,0,0,0,99" `shouldBe` Right "2,0,0,0,99"
      runIntcode "2,3,0,3,99" `shouldBe` Right "2,3,0,6,99"
      runIntcode "2,4,4,5,99,0" `shouldBe` Right "2,4,4,5,99,9801"
      runIntcode "1,1,1,4,99,5,6,0,99" `shouldBe` Right "30,1,1,4,2,5,6,0,99"
      runIntcode "1,5,0,0,99" `shouldBe` Left "Invalid address"
      runIntcode "1,0,5,0,99" `shouldBe` Left "Invalid address"
      runIntcode "1,0,0,5,99" `shouldBe` Left "Invalid address"
      runIntcode "1" `shouldBe` Left "Invalid address"
      runIntcode "1,0" `shouldBe` Left "Invalid address"
      runIntcode "1,0,0" `shouldBe` Left "Invalid address"
      runIntcode "1,0,0,0" `shouldBe` Left "Invalid address"

runIntcode :: Text -> Either String Text
runIntcode sourceCode = do
  program <- parseProgram sourceCode
  newProgram <- runProgram program
  return $ unparseProgram newProgram
