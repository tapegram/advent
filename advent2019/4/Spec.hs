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

type Noun = Integer
type Verb = Integer
type Output = Integer
type Program = [Integer]
data Result noun verb output = Result noun verb output deriving (Show, Eq)

program :: Program
program = [
        1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,1,9,23,27,2,27,9,31,1,31,5,35,2,35,9,39,1,39,10,43,2,43,13,47,1,47,6,51,2,51,10,55,1,9,55,59,2,6,59,63,1,63,6,67,1,67,10,71,1,71,10,75,2,9,75,79,1,5,79,83,2,9,83,87,1,87,9,91,2,91,13,95,1,95,9,99,1,99,6,103,2,103,6,107,1,107,5,111,1,13,111,115,2,115,6,119,1,119,5,123,1,2,123,127,1,6,127,0,99,2,14,0,0
          ]
createProgram :: Noun -> Verb -> Program -> Program
createProgram noun verb (x : _ : _ : xs) = x : noun : verb : xs
createProgram _ _ _ = error "Bad Program"

getAllResults :: Program -> Noun -> [Result Noun Verb Output]
getAllResults p noun
  | noun < 100               = getAllResultsForVerb p noun 0 ++ getAllResults p (noun+1)
  | otherwise  = []

getAllResultsForVerb :: Program -> Noun -> Verb -> [Result Noun Verb Output]
getAllResultsForVerb p noun verb
  | verb < 100 = Result noun verb (index 0 (run 0 $ createProgram noun verb p))
                  : getAllResultsForVerb p noun (verb+1)
  | otherwise  = []

findInputForOutput :: Output -> [Result Noun Verb Output] -> Result Noun Verb Output
findInputForOutput _ [] = error "Ran out of elements to check"
findInputForOutput output ((Result noun verb rOutput) : rs)
  | output == rOutput = Result noun verb rOutput
  | otherwise         = findInputForOutput output rs


main :: IO ()
main = hspec $ do
  describe "Find input for output" $ do
    it "19690720" $ do
      findInputForOutput 19690720 (getAllResults program 0) `shouldBe` Result 25 5 19690720

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
    it "Full ship program" $ do
      run 0 program `shouldBe` [
        9581917,12,2,2,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,48,2,9,19,144,1,9,23,147,2,27,9,441,1,31,5,442,2,35,9,1326,1,39,10,1330,2,43,13,6650,1,47,6,6652,2,51,10,26608,1,9,55,26611,2,6,59,53222,1,63,6,53224,1,67,10,53228,1,71,10,53232,2,9,75,159696,1,5,79,159697,2,9,83,479091,1,87,9,479094,2,91,13,2395470,1,95,9,2395473,1,99,6,2395475,2,103,6,4790950,1,107,5,4790951,1,13,111,4790956,2,115,6,9581912,1,119,5,9581913,1,2,123,9581915,1,6,127,0,99,2,14,0,0
        ]
