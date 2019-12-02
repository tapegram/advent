import Test.Hspec
import Prelude

data Opcode l1 l2 l3 = Terminate | Add l1 l2 l3 | Multiply l1 l2 l3 deriving (Show, Eq)

getOpcode :: [Integer] -> Opcode Integer Integer Integer
getOpcode (1: b: c: d: _) = Add b c d
getOpcode (2: b: c: d: _) = Multiply b c d
getOpcode _ = Terminate

replace :: Integer -> Integer -> [Integer] -> [Integer]
replace i val list = let (x, _ : xs) = splitAt (fromIntegral i) list in x ++ val : xs

index :: Integer -> [Integer] -> Integer
index i (x:xs) = (x:xs)!!(fromIntegral i)
index _ [] = error "Bad"

processOpcode :: Opcode Integer Integer Integer -> [Integer] -> [Integer]
processOpcode (Add l1 l2 l3) (x:xs)      = let val = (index l1 (x:xs)) + (index l2 (x:xs))
                                           in replace l3 val (x:xs)

processOpcode (Multiply l1 l2 l3) (x:xs) = let val = (index l1 (x:xs)) * (index l2 (x:xs))
                                           in replace l3 val (x:xs)

processOpcode Terminate (x:xs)           = (x:xs)
processOpcode _ _                        = []

run :: Integer -> [Integer] -> [Integer]
run _ [] = []
run i (x:xs)
  | opCode == Terminate = (x:xs)
  | otherwise           = run (i+4) (processOpcode(opCode) (x:xs))
  where opCode = getOpcode $ drop (fromIntegral i) (x:xs)

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
      processOpcode (Add 1 2 3) [1, 2, 3, 4] `shouldBe` [1, 2, 3, 5]
    it "Multiply" $ do
      processOpcode (Multiply 1 2 3) [1, 2, 3, 4] `shouldBe` [1, 2, 3, 6]
    it "Terminate" $ do
      processOpcode Terminate [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

  describe "Run program" $ do
    it "[]" $ do
      run 0 [] `shouldBe` []
    it "[1,0,0,0,99]" $ do
      run 0 [1, 0, 0, 0, 99] `shouldBe` [2,0,0,0,99]
    it "[2,3,0,3,99]" $ do
      run 0 [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
    it "[2,4,4,5,99,0]" $ do
      run 0 [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
    it "[1,1,1,4,99,5,6,0,99]" $ do
      run 0 [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
    it "Full ship code" $ do
      run 0 [
        1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0
        ] `shouldBe` [
        250715,0,0,2,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,0,2,9,19,0,1,9,23,3,2,27,9,9,1,31,5,10,2,35,9,30,1,39,10,34,2,43,13,170, 1,47,6,172,2,51,10,688,1,9,55,691,2,6,59,1382,1,63,6,1384,1,67,10,1388,1,71,10,1392,2,9,75,4176,1,5,79,4177,2,9,83,12531,1,87,9,12534, 2,91,13,62670,1,95,9,62673,1,99,6,62675,2,103,6,125350,1,107,5,125351,1,13,111,125356,2,115,6,250712,1,119,5,250713,1,2,123,250713,1,6,127,0,99,2,14,0,0
        ]
