import Test.Hspec
import Prelude

type Distance = Integer
data Vector scalar = R scalar | L scalar | U scalar | D scalar deriving (Show, Eq)
data Coord x y = Coord x y deriving (Show, Eq)
type Path = [Coord Integer Integer]

-- getDistanceFromPortToOverlap :: [Vector Distance] -> Distance
-- getDistanceFromPortToOverlap _ = 0


findIntersections :: [Path] -> [Coord Integer Integer]
findIntersections _ = []

main :: IO ()
main = hspec $ do
  -- describe "GetDistanceFromPortToOverlap" $ do
  --   it "example 1" $ do
  --     getDistanceFromPortToOverlap [
  --       R 75,D 30,R 83,U 83, L 12, D 49, R 71, U 7, L 72, U 62, R 66,U 55, R 34, D 71, R 55, D 58, R 83
  --                                  ]
  --       `shouldBe` 159

  describe "findIntersections" $ do
    it "no paths should return no intersections" $ do
      findIntersections [] `shouldBe` []
    it "one path should return no intersections" $ do
      findIntersections [[Coord 0 0, Coord 1 0, Coord 2 2]] `shouldBe` []
