import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  let initialState = [ moon [-1,   0,  2]
                     , moon [ 2, -10, -7]
                     , moon [ 4,  -8,  8]
                     , moon [ 3,   5, -1]
                     ]
  let simulation = simulate initialState

  describe "simulate" $
    it "returns an infinite list of states" $ do
      simulation !! 1 `shouldBe` [ Moon { position = [2, -1,  1], velocity = [ 3, -1, -1] }
                                 , Moon { position = [3, -7, -4], velocity = [ 1,  3,  3] }
                                 , Moon { position = [1, -7,  5], velocity = [-3,  1, -3] }
                                 , Moon { position = [2,  2,  0], velocity = [-1, -3,  1] }
                                 ]
      simulation !! 10 `shouldBe` [ Moon { position = [2,  1, -3], velocity = [-3, -2,  1] }
                                  , Moon { position = [1, -8,  0], velocity = [-1,  1,  3] }
                                  , Moon { position = [3, -6,  1], velocity = [ 3,  2, -3] }
                                  , Moon { position = [2,  0,  4], velocity = [ 1, -1, -1] }
                                  ]

  describe "cycleLength" $
    it "returns the number of steps necessary to get back to the initial state" $ do
      cycleLength simulation `shouldBe` 2772
      cycleLength' simulation `shouldBe` 2772
