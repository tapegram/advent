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
manhattanDistance (Coord x y) (Coord x' y') = (abs $ x' - x) + (abs $ y' - y)

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

    it "y difference 1" $ do
      manhattanDistance (Coord 0 0) (Coord 0 1) `shouldBe` 1
    it "y difference 2" $ do
      manhattanDistance (Coord 5 7) (Coord 5 9) `shouldBe` 2

    it "y negative difference" $ do
      manhattanDistance (Coord 1 0) (Coord 1 (-5)) `shouldBe` 5
    it "y negative difference 2" $ do
      manhattanDistance (Coord 1 (-1)) (Coord 1 (-5)) `shouldBe` 4
    it "y negative difference 3" $ do
      manhattanDistance (Coord 0 (-7)) (Coord 0 (-1)) `shouldBe` 6
    it "y negative difference 4" $ do
      manhattanDistance (Coord 0 (-2)) (Coord 0 1) `shouldBe` 3

    it "x difference 1" $ do
      manhattanDistance (Coord 0 0) (Coord 1 0) `shouldBe` 1
    it "x difference 2" $ do
      manhattanDistance (Coord 7 5) (Coord 9 5) `shouldBe` 2

    it "x negative difference" $ do
      manhattanDistance (Coord 0 1) (Coord (-5) 1) `shouldBe` 5
    it "x negative difference 2" $ do
      manhattanDistance (Coord (-1) 1) (Coord (-5) 1) `shouldBe` 4
    it "x negative difference 3" $ do
      manhattanDistance (Coord (-7) 0) (Coord (-1) 0) `shouldBe` 6
    it "x negative difference 4" $ do
      manhattanDistance (Coord (-2) 0) (Coord 1 0) `shouldBe` 3


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
