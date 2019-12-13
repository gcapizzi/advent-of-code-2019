import Test.Hspec
import Lib

main :: IO ()
main = hspec $
  describe "isValidPassword" $
    it "validates a password" $ do
      isValidPassword "122345" `shouldBe` True
      isValidPassword "112233" `shouldBe` True
      isValidPassword "111122" `shouldBe` True
      isValidPassword "111123" `shouldBe` False
      isValidPassword "223450" `shouldBe` False
      isValidPassword "123789" `shouldBe` False
      isValidPassword "123444" `shouldBe` False
