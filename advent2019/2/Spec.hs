import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Prelude

type Mass = Double

calcFuelForMass :: Mass -> Int
calcFuelForMass m
  | fuelMass > 0 = fuelMass + (calcFuelForMass (fromIntegral fuelMass))
  | otherwise    = 0
  where fuelMass = floor (m / 3) - 2


calcFuelForMasses :: [Mass] -> Int
calcFuelForMasses (m:ms) = sum $ map (calcFuelForMass) (m:ms)


main :: IO ()
main = hspec $ do
  describe "Calculates fuel requirements" $ do
    it "calculates the value for 12 units" $ do
      calcFuelForMass 12 `shouldBe` (2 :: Int)
    it "calculates the value for 14 units" $ do
      calcFuelForMass 14 `shouldBe` (2 :: Int)
    it "calculates the value for 1969 units" $ do
      calcFuelForMass 1969 `shouldBe` (966 :: Int)
    it "calculates the value for 100756 units" $ do
      calcFuelForMass 100756 `shouldBe` (50346 :: Int)

  describe "Calculates fuel requirement for a ship" $ do
    it "calculates the value for 12 and 14 units" $ do
      calcFuelForMasses [12, 14] `shouldBe` (4 :: Int)
    it "calculates the value for 12, 14, and 1969 units" $ do
      calcFuelForMasses [12, 14, 1969] `shouldBe` (970:: Int)
    it "calculates the value for the full ship" $ do
      calcFuelForMasses [
        60566,
        53003,
        132271,
        130557,
        109138,
        64818,
        123247,
        148493,
        98275,
        67155,
        132365,
        133146,
        88023,
        92978,
        122790,
        84429,
        93421,
        76236,
        104387,
        135953,
        131379,
        125949,
        133614,
        94647,
        64289,
        87972,
        97331,
        132327,
        53913,
        79676,
        143110,
        79269,
        52366,
        62793,
        69437,
        97749,
        83596,
        147597,
        115883,
        82062,
        63800,
        61521,
        139314,
        127619,
        85790,
        132960,
        141289,
        86146,
        146104,
        128708,
        133054,
        116777,
        128402,
        85043,
        117344,
        107915,
        108669,
        108304,
        105300,
        75186,
        111352,
        112936,
        117177,
        93812,
        97737,
        61835,
        77529,
        145406,
        93489,
        75642,
        69806,
        109845,
        79133,
        60950,
        67797,
        111806,
        50597,
        50481,
        88338,
        102136,
        65377,
        55982,
        82754,
        68901,
        89232,
        63118,
        95534,
        98264,
        147706,
        80050,
        104953,
        146758,
        122884,
        122024,
        129236,
        113818,
        58099,
        134318,
        136312,
        75124
                        ] `shouldBe` (4973628 :: Int)
