import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "countOrbits" $
    it "calculates the tree score" $ do
      countOrbits "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n" `shouldBe` 42
      countOrbits "K)L\nCOM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\n" `shouldBe` 42

  describe "orbitalTransfers" $
    it "calculates the number of orbital transfers between two nodes" $
      orbitalTransfers "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n" `shouldBe` Just 4
