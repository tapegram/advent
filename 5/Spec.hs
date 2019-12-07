import Test.Hspec
import Prelude
import Data.List

type Distance = Integer
data Vector scalar = R scalar | L scalar | U scalar | D scalar deriving (Show, Eq)
data Coord x y = Coord x y deriving (Show, Eq)
type Wire = [Coord Integer Integer]
data TwoWires a b = TwoWires a b deriving (Show, Eq)

-- getDistanceFromPortToOverlap :: [Vector Distance] -> Distance
-- getDistanceFromPortToOverlap _ = 0


findIntersections :: TwoWires Wire Wire-> [Coord Integer Integer]
findIntersections (TwoWires x y) = intersect x y

manhattanDistance :: Coord Integer Integer -> Coord Integer Integer -> Distance
manhattanDistance _ _ = 0

main :: IO ()
main = hspec $ do
  -- describe "GetDistanceFromPortToOverlap" $ do
  --   it "example 1" $ do
  --     getDistanceFromPortToOverlap [
  --       R 75,D 30,R 83,U 83, L 12, D 49, R 71, U 7, L 72, U 62, R 66,U 55, R 34, D 71, R 55, D 58, R 83
  --                                  ]
  --       `shouldBe` 159
  describe "manhattanDistance" $ do

    it "Same coordinate" $ do
      manhattanDistance (Coord 0 0) (Coord 0 0) `shouldBe` 0
    it "Same coordinate 2" $ do
      manhattanDistance (Coord 1 1) (Coord 1 1) `shouldBe` 0
    it "Same coordinate 3" $ do
      manhattanDistance (Coord 5 99) (Coord 5 99) `shouldBe` 0

  describe "findIntersections" $ do

    it "no intersections" $ do
      findIntersections (
        TwoWires
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 2 0, Coord 2 1, Coord 2 2]
        )
         `shouldBe` []

    it "one intersection" $ do
      findIntersections (
        TwoWires
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 2 0, Coord 1 1, Coord 2 2]
        )
         `shouldBe` [Coord 1 1]

    it "two intersections" $ do
      findIntersections (
        TwoWires
        [Coord 1 0, Coord 1 1, Coord 1 2, Coord 2 0]
        [Coord 2 0, Coord 2 1, Coord 2 2, Coord 1 1]
        )
         `shouldBe` [Coord 1 1, Coord 2 0]

    it "all values are intersections" $ do
      findIntersections (
        TwoWires
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 1 0, Coord 1 1, Coord 1 2]
        )
         `shouldBe` [Coord 1 0, Coord 1 1, Coord 1 2]
