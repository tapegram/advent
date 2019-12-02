import Test.Hspec
import Prelude

data Opcode l1 l2 l3 = Terminate | Add l1 l2 l3 | Multiply l1 l2 l3 deriving (Show, Eq)

getOpcode :: [Integer] -> Opcode Integer Integer Integer
getOpcode (1: b: c: d: _) = Add b c d
getOpcode (2: b: c: d: _) = Multiply b c d
getOpcode _ = Terminate

replace :: Integer -> a -> [a] -> [a]
replace i val list = let (x, _ : xs) = splitAt (fromIntegral i) list in x ++ val : xs

processOpcode :: Opcode Integer Integer Integer -> [Integer] -> [Integer]
processOpcode (Add l1 l2 l3) (x:xs)  = replace l3 ((x:xs)!!(fromIntegral l1) + (x:xs)!!(fromIntegral l2)) (x:xs)
processOpcode _ _ = []

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

  describe "Replace" $ do
    it "Replace Integer in the middle" $ do
      replace 2 99 [1, 2, 3, 4] `shouldBe` [1, 2, 99, 4]
    it "Replace Integer at the start" $ do
      replace 0 99 [1, 2, 3, 4] `shouldBe` [99, 2, 3, 4]
    it "Replace Integer at the end" $ do
      replace 3 99 [1, 2, 3, 4] `shouldBe` [1, 2, 3, 99]

  describe "Process Opcode" $ do
    it "Add" $ do
      processOpcode (Add 1 2 3) [1, 2, 3, 4] `shouldBe` [1, 2, 3, 8]

  describe "Run program" $ do
    it "[1,0,0,0,99]" $ do
      run [1, 0, 0, 0, 99] `shouldBe` [2,0,0,0,99]
