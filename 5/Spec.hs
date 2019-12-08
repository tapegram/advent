import Test.Hspec
import Prelude
import qualified Data.Set as Set

type Distance = Integer
data Vector scalar = R scalar | L scalar | U scalar | D scalar deriving (Show, Eq)
data Coord x y = Coord x y deriving (Show, Eq, Ord)
type Wire = [Coord Integer Integer]
data TwoWires a b = TwoWires a b deriving (Show, Eq)

wireFromVectors :: Coord Integer Integer -> [Vector Distance] -> Wire

wireFromVectors c ((U 0) : vs) = wireFromVectors c vs
wireFromVectors (Coord x y) ((U s) : vs) = (Coord x (y+1)) : wireFromVectors (Coord x (y+1)) ((U (s-1)) : vs)

wireFromVectors c ((D 0) : vs) = wireFromVectors c vs
wireFromVectors (Coord x y) ((D s) : vs) = (Coord x (y-1)) : wireFromVectors (Coord x (y-1)) ((D (s-1)) : vs)

wireFromVectors c ((L 0) : vs) = wireFromVectors c vs
wireFromVectors (Coord x y) ((L s) : vs) = (Coord (x-1) y) : wireFromVectors (Coord (x-1) y) ((L (s-1)) : vs)

wireFromVectors c ((R 0) : vs) = wireFromVectors c vs
wireFromVectors (Coord x y) ((R s) : vs) = (Coord (x+1) y) : wireFromVectors (Coord (x+1) y) ((R (s-1)) : vs)

wireFromVectors _ [] = []

distanceToClosestIntersection :: Coord Integer Integer -> Wire -> Wire -> Distance
distanceToClosestIntersection (Coord x y) w1 w2 =
  minimum $ map (manhattanDistance (Coord x y)) $ findIntersections w1 w2

findIntersections :: Wire -> Wire -> [Coord Integer Integer]
findIntersections x y = Set.toList $ Set.intersection (Set.fromList x) (Set.fromList y)

manhattanDistance :: Coord Integer Integer -> Coord Integer Integer -> Distance
manhattanDistance (Coord x y) (Coord x' y') = (abs $ x' - x) + (abs $ y' - y)

main :: IO ()
main = hspec $ do
  describe "GetDistanceFromPortToOverlap" $ do
    it "example 1" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        (wireFromVectors (Coord 0 0) [R 75,D 30,R 83,U 83, L 12, D 49, R 71, U 7, L 72])
        (wireFromVectors (Coord 0 0) [U 62, R 66,U 55, R 34, D 71, R 55, D 58, R 83])
        `shouldBe` 159

    it "example 2" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        (wireFromVectors (Coord 0 0) [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51])
        (wireFromVectors (Coord 0 0) [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7])
        `shouldBe` 135

    it "the problem" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        (wireFromVectors (Coord 0 0) [
                      R 998, U 502, R 895, D 288, R 416, U 107, R 492, U 303, R 719, D 601, R 783, D 154, L 236, U 913, R 833, D 329, R 28, D 759, L 270, D 549, L 245, U 653, L 851, U 676, L 211, D 949, R 980, U 314, L 897, U 764, R 149, D 214, L 195, D 907, R 534, D 446, R 362, D 6, L 246, D 851, L 25, U 925, L 334, U 673, L 998, U 581, R 783, U 912, R 53, D 694, L 441, U 411, L 908, D 756, R 946, D 522, L 77, U 468, R 816, D 555, L 194, D 707, R 97, D 622, R 99, D 265, L 590, U 573, R 132, D 183, L 969, D 207, L 90, D 331, R 88, D 606, L 315, U 343, R 546, U 460, L 826, D 427, L 232, U 117, R 125, U 309, R 433, D 53, R 148, U 116, L 437, U 339, L 288, D 879, L 52, D 630, R 201, D 517, L 341, U 178, R 94, U 636, L 759, D 598, L 278, U 332, R 192, U 463, L 325, U 850, L 200, U 810, L 686, U 249, L 226, D 297, R 915, D 117, R 56, D 59, R 760, U 445, R 184, U 918, R 173, D 903, R 212, D 868, L 88, D 798, L 829, U 835, L 563, U 19, R 480, D 989, R 529, D 834, R 515, U 964, L 876, D 294, R 778, D 551, L 457, D 458, R 150, D 698, R 956, D 781, L 310, D 948, R 50, U 56, R 98, U 348, L 254, U 614, L 654, D 359, R 632, D 994, L 701, D 615, R 64, D 507, R 668, D 583, L 687, D 902, L 564, D 214, R 930, D 331, L 212, U 943, R 559, U 886, R 590, D 805, R 426, U 669, L 141, D 233, L 573, D 682, L 931, U 267, R 117, D 900, L 944, U 667, L 838, D 374, L 406, U 856, R 987, D 870, R 716, D 593, R 596, D 654, R 653, U 120, L 666, U 145, R 490, D 629, R 172, D 881, L 808, D 324, R 956, D 532, L 475, U 165, L 503, U 361, R 208, U 323, R 568, D 876, R 663, D 11, L 839, D 67, R 499, U 75, L 643, U 954, R 94, D 418, R 761, D 842, L 213, D 616, L 785, D 42, R 707, D 343, L 513, D 480, L 531, D 890, L 899, D 2, L 30, D 188, R 32, U 588, R 480, U 33, R 849, U 443, L 666, U 117, L 13, D 974, L 453, U 93, R 960, D 369, R 332, D 61, L 17, U 557, R 818, D 744, L 124, U 916, L 454, D 572, R 451, D 29, R 711, D 134, R 481, U 366, L 327, U 132, L 819, U 839, R 485, U 941, R 224, U 531, R 688, U 561, R 958, D 899, L 315, U 824, L 408, D 941, R 517, D 163, L 878, U 28, R 767, D 798, R 227
                                               ])
        (wireFromVectors (Coord 0 0) [
                      L 1009, U 399, R 373, U 980, L 48, U 638, R 725, U 775, R 714, D 530, L 887, D 576, L 682, D 940, L 371, D 621, L 342, D 482, R 676, D 445, R 752, U 119, L 361, D 444, L 769, D 854, L 874, U 259, R 332, U 218, R 866, U 28, L 342, D 233, L 958, U 649, R 998, U 262, L 8, D 863, L 283, D 449, L 73, D 438, L 516, D 54, R 964, D 981, R 338, U 332, L 761, U 704, L 705, D 468, L 115, U 834, R 367, D 156, R 480, U 27, R 846, U 73, R 846, D 720, R 811, D 466, L 407, U 928, R 816, U 50, R 90, D 893, L 930, D 833, L 159, D 972, L 823, U 868, R 689, D 558, L 777, D 13, R 844, D 8, L 168, U 956, L 111, D 462, L 667, U 559, L 839, U 503, R 906, D 838, R 83, D 323, L 782, U 588, R 599, D 233, L 700, U 679, L 51, U 779, L 110, D 260, L 201, U 992, L 43, D 557, L 628, D 875, L 201, U 535, L 246, D 976, L 546, D 22, R 600, D 301, L 542, D 41, R 532, U 316, L 765, D 310, L 666, D 369, R 853, U 684, L 457, U 816, L 667, U 758, R 798, U 959, R 893, D 185, L 842, U 168, R 68, D 348, R 394, D 296, R 966, D 511, L 319, U 717, L 57, U 129, R 843, U 439, L 744, D 870, L 162, D 991, R 77, D 565, R 494, U 601, L 851, U 748, L 96, U 124, L 379, D 446, L 882, U 371, R 133, U 820, L 935, D 704, L 670, D 911, L 182, U 138, R 844, U 926, L 552, D 716, L 849, U 624, R 723, U 117, R 252, D 737, L 216, U 796, R 156, U 322, R 812, D 390, L 50, D 493, L 665, U 314, L 584, U 798, L 11, U 524, R 171, U 837, R 981, U 32, L 277, U 650, L 865, U 28, R 399, U 908, R 652, D 543, L 779, D 406, L 839, D 198, L 190, D 319, L 776, U 752, R 383, D 884, R 385, D 682, R 729, D 163, R 252, U 533, L 690, D 767, R 533, D 147, R 366, U 716, R 548, U 171, R 932, U 720, L 9, D 39, R 895, U 850, L 276, D 988, L 528, U 551, L 262, D 480, L 275, D 567, R 70, D 599, L 814, U 876, R 120, U 93, L 565, U 795, L 278, D 41, R 695, D 693, R 208, U 272, L 923, U 498, R 238, U 268, L 244, U 278, R 965, U 395, R 990, U 329, L 478, D 245, R 980, D 473, L 702, U 396, R 358, U 636, R 400, D 919, R 240, U 780, L 251, D 633, L 55, D 723, L 529, U 319, L 299, D 89, L 251, D 557, L 705, D 705, L 391, D 58, R 241
                                               ])
        `shouldBe` 1337

  describe "wireFromVectors" $ do

    it "up 3" $ do
      wireFromVectors (Coord 0 0) [U 3]
        `shouldBe` [
        Coord 0 1,
        Coord 0 2,
        Coord 0 3
                   ]
    it "down 2" $ do
      wireFromVectors (Coord 0 0) [D 2]
        `shouldBe` [
        Coord 0 (-1),
        Coord 0 (-2)
                   ]

    it "left 4" $ do
      wireFromVectors (Coord 0 0) [L 4]
        `shouldBe` [
        Coord (-1) 0,
        Coord (-2) 0,
        Coord (-3) 0,
        Coord (-4) 0
                   ]

    it "right 1" $ do
      wireFromVectors (Coord 0 0) [R 1]
        `shouldBe` [Coord 1 0]

    it "do a lap" $ do
      wireFromVectors (Coord 0 0) [U 3, R 2, D 4, L 1]
        `shouldBe` [
        Coord 0 1,
        Coord 0 2,
        Coord 0 3,
        Coord 1 3,
        Coord 2 3,
        Coord 2 2,
        Coord 2 1,
        Coord 2 0,
        Coord 2 (-1),
        Coord 1 (-1)
                   ]

  describe "distanceToClosestIntersection" $ do

    it "one intersection on origin" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        [Coord 0 0, Coord 0 1, Coord 0 2]
        [Coord 0 0, Coord 1 0, Coord 2 0]
        `shouldBe` 0

    it "one intersection" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        [Coord 0 0, Coord 1 1, Coord 0 2]
        [Coord 1 1, Coord 2 0, Coord 3 0]
        `shouldBe` 2

    it "two intersections" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        [Coord 0 0, Coord 1 1, Coord 2 2]
        [Coord 1 1, Coord 2 2, Coord 3 0]
        `shouldBe` 2

    it "three intersections" $ do
      distanceToClosestIntersection
        (Coord 0 0)
        [Coord (-1) 0, Coord 1 1, Coord 2 2]
        [Coord (-1) 0, Coord 1 1, Coord 2 2]
        `shouldBe` 1

    it "three intersections with a different origin" $ do
      distanceToClosestIntersection
        (Coord 3 4)
        [Coord (-1) 0, Coord 1 1, Coord 2 2]
        [Coord (-1) 0, Coord 1 1, Coord 2 2]
        `shouldBe` 3

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

    it "+/+ +/+" $ do
      manhattanDistance (Coord 3 4) (Coord 2 1) `shouldBe` 4
    it "-/+ +/+" $ do
      manhattanDistance (Coord (-3) 4) (Coord 2 1) `shouldBe` 8
    it "+/- +/+" $ do
      manhattanDistance (Coord 3 (-4)) (Coord 2 1) `shouldBe` 6


  describe "findIntersections" $ do

    it "no intersections" $ do
      findIntersections
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 2 0, Coord 2 1, Coord 2 2]
         `shouldBe` []

    it "one intersection" $ do
      findIntersections
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 2 0, Coord 1 1, Coord 2 2]
         `shouldBe` [Coord 1 1]

    it "two intersections" $ do
      findIntersections
        [Coord 1 0, Coord 1 1, Coord 1 2, Coord 2 0]
        [Coord 2 0, Coord 2 1, Coord 2 2, Coord 1 1]
         `shouldBe` [Coord 1 1, Coord 2 0]

    it "all values are intersections" $ do
      findIntersections
        [Coord 1 0, Coord 1 1, Coord 1 2]
        [Coord 1 0, Coord 1 1, Coord 1 2]
         `shouldBe` [Coord 1 0, Coord 1 1, Coord 1 2]
