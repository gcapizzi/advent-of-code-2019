import Test.Hspec
import Intcode
import Data.Text (Text)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

main :: IO ()
main = hspec $
  describe "run" $
    it "is a complete Incode computer" $ do
      instructions <$> runSrc "1,0,0,0,99" `shouldBe` Right (fromList [2,0,0,0,99])
      instructions <$> runSrc "2,3,0,3,99" `shouldBe` Right (fromList [2,3,0,6,99])
      instructions <$> runSrc "2,4,4,5,99,0" `shouldBe` Right (fromList [2,4,4,5,99,9801])
      instructions <$> runSrc "1,1,1,4,99,5,6,0,99" `shouldBe` Right (fromList [30,1,1,4,2,5,6,0,99])

      runSrc "1,-1,0,0,99" `shouldBe` Left "Invalid address"
      runSrc "1,0,-1,0,99" `shouldBe` Left "Invalid address"
      runSrc "1,0,0,-1,99" `shouldBe` Left "Invalid address"

      instructions <$> runSrc "1002,4,3,4,33" `shouldBe` Right (fromList [1002,4,3,4,99])

      outputs <$> runSrcWithInputs "3,5,4,5,99,0" [42] `shouldBe` Right [42]
      outputs <$> runSrcWithInputs "3,9,8,9,10,9,4,9,99,-1,8" [8] `shouldBe` Right [1]
      outputs <$> runSrcWithInputs "3,9,8,9,10,9,4,9,99,-1,8" [7] `shouldBe` Right [0]

      address <$> runSrcWithInputs "3,5,4,5,99,0" [] `shouldBe` Right 0

      instructions <$> runSrc "1202,4,3,4,33" `shouldBe` Right (fromList [1202,4,3,4,99])
      instructions <$> runSrc "109,-5,1202,11,3,6,33" `shouldBe` Right (fromList [109,-5,1202,11,3,6,99])
      instructions <$> runSrc "109,-5,21202,11,3,11,33" `shouldBe` Right (fromList [109,-5,21202,11,3,11,99])

      instructions <$> runSrc "1001,5,42,5,99" `shouldBe` Right (fromList [1001,5,42,5,99,42])

      outputs <$> runSrc "1102,34915192,34915192,7,4,7,99,0" `shouldBe` Right [1219070632396864]
      outputs <$> runSrc "104,1125899906842624,99" `shouldBe` Right [1125899906842624]

      outputs <$> runSrc "104,1,104,2,104,3,99" `shouldBe` Right [1,2,3]

runSrcWithInputs :: Text -> [Int] -> Either String Program
runSrcWithInputs src inputs = do
    program <- parse src
    run program { inputs = inputs }

runSrc :: Text -> Either String Program
runSrc src = runSrcWithInputs src []

fromList :: [Int] -> IntMap Int
fromList = M.fromList . zip [0..]
