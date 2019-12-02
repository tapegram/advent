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
