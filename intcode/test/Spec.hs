import Test.Hspec
import Intcode
import qualified Data.Vector as V
import Data.Text (Text)

main :: IO ()
main = hspec $ do
  describe "run" $
    it "receives inputs and returns outputs" $ do
      instructions <$> runSrc "1,0,0,0,99" `shouldBe` Right (V.fromList [2,0,0,0,99])
      instructions <$> runSrc "2,3,0,3,99" `shouldBe` Right (V.fromList [2,3,0,6,99])
      instructions <$> runSrc "2,4,4,5,99,0" `shouldBe` Right (V.fromList [2,4,4,5,99,9801])
      instructions <$> runSrc "1,1,1,4,99,5,6,0,99" `shouldBe` Right (V.fromList [30,1,1,4,2,5,6,0,99])

      runSrc "1,5,0,0,99" `shouldBe` Left "Invalid address"
      runSrc "1,0,5,0,99" `shouldBe` Left "Invalid address"
      runSrc "1,0,0,5,99" `shouldBe` Left "Invalid address"
      runSrc "1" `shouldBe` Left "Invalid address"
      runSrc "1,0" `shouldBe` Left "Invalid address"
      runSrc "1,0,0" `shouldBe` Left "Invalid address"
      runSrc "1,0,0,0" `shouldBe` Left "Invalid address"

      instructions <$> runSrc "1002,4,3,4,33" `shouldBe` Right (V.fromList [1002,4,3,4,99])

      outputs <$> runSrcWithInputs "3,5,4,5,99,0" [42] `shouldBe` Right [42]
      outputs <$> runSrcWithInputs "3,9,8,9,10,9,4,9,99,-1,8" [8] `shouldBe` Right [1]
      outputs <$> runSrcWithInputs "3,9,8,9,10,9,4,9,99,-1,8" [7] `shouldBe` Right [0]

      address <$> runSrcWithInputs "3,5,4,5,99,0" [] `shouldBe` Right 0

runSrcWithInputs :: Text -> [Int] -> Either String Program
runSrcWithInputs src inputs = do
    program <- parse src
    run program { inputs = inputs }

runSrc :: Text -> Either String Program
runSrc src = runSrcWithInputs src []
