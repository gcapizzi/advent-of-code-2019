import Test.Hspec
import Lib
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "parseAsteroids" $
    it "parses an asteroids field" $
      parseAsteroids ".#.\n..#\n#..\n" `shouldBe` S.fromList [Asteroid 1 0, Asteroid 0 2, Asteroid 2 1]

  describe "neighbours" $
    it "lists the vaporizables asteroids in order" $ do
      let txt = T.unlines [ ".#..##.###...#######"
                          , "##.############..##."
                          , ".#.######.########.#"
                          , ".###.#######.####.#."
                          , "#####.##.#.##.###.##"
                          , "..#####..#.#########"
                          , "####################"
                          , "#.####....###.#.#.##"
                          , "##.#################"
                          , "#####.##.###..####.."
                          , "..######..##.#######"
                          , "####.##.####...##..#"
                          , ".#####..#.######.###"
                          , "##...#.##########..."
                          , "#.##########.#######"
                          , ".####.#.###.###.#.##"
                          , "....##.##.###..#####"
                          , ".#.#.###########.###"
                          , "#.#.#.#####.####.###"
                          , "###.##.####.##.#..##"
                          ]
      let asteroids = parseAsteroids txt
      let vaporizables = neighbours asteroids (Asteroid 11 13)

      vaporizables !!   0 `shouldBe` Asteroid 11 12
      vaporizables !!   1 `shouldBe` Asteroid 12  1
      vaporizables !!   2 `shouldBe` Asteroid 12  2
      vaporizables !!   9 `shouldBe` Asteroid 12  8
      vaporizables !!  19 `shouldBe` Asteroid 16  0
      vaporizables !!  49 `shouldBe` Asteroid 16  9
      vaporizables !!  99 `shouldBe` Asteroid 10 16
      vaporizables !! 198 `shouldBe` Asteroid  9  6
      vaporizables !! 199 `shouldBe` Asteroid  8  2
      vaporizables !! 200 `shouldBe` Asteroid 10  9
      vaporizables !! 298 `shouldBe` Asteroid 11  1
