import Test.Hspec
import Prelude

data Opcode l1 l2 l3 = Terminate | Add l1 l2 l3 | Multiply l1 l2 l3 deriving (Show, Eq)

getOpcode :: [Integer] -> Opcode Integer Integer Integer
getOpcode (1: b: c: d:_) = Add b c d
getOpcode (2: b: c: d:_) = Multiply b c d
getOpcode _ = Terminate

run :: [Integer] -> [Integer]
run [] = []
run (x:xs) = (x:xs)

main :: IO ()
main = hspec $ do
  describe "getOpcode" $ do
    it "get Add" $ do
      getOpcode [1, 0, 0, 0, 99] `shouldBe` Add 0 0 0
    it "get Multiple" $ do
      getOpcode [2, 1, 2, 3, 99] `shouldBe` Multiply 1 2 3
    it "Terminate on 99" $ do
      getOpcode [99, 1, 2, 3, 4] `shouldBe` Terminate
    it "Terminate on not enough remaining values" $ do
      getOpcode [1, 2, 3] `shouldBe` Terminate
  describe "Run program" $ do
    it "[1,0,0,0,99]" $ do
      run [1, 0, 0, 0, 99] `shouldBe` [2,0,0,0,99]
