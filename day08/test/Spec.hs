import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "rendering" $
    it "merges the image layers" $ do
      renderImage (parseImage 3 3 "022202220220212022") `shouldBe` "020\n202\n020\n"
      renderImage (parseImage 2 2 "0222112222120000") `shouldBe` "01\n10\n"
