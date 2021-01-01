import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.IntMap.Strict as Map
import Data.Bits ( Bits(rotate, clearBit, setBit) )
import Data.Either ( fromRight )

type Program = [Text.Text]
type Memory = Map.IntMap Int
type Mask = Text.Text
type Instruction = Text.Text

defaultMask :: Mask
defaultMask = Text.pack ""

instructionDelimiter :: Text.Text
instructionDelimiter = Text.pack " = "

readError :: (Int, Text.Text)
readError = (0, Text.pack "")

hasMaskPrefix :: Text.Text -> Bool
hasMaskPrefix = (== Text.pack "mas")

mask :: Instruction -> Mask
mask = flip (!!) 1 . Text.splitOn instructionDelimiter

readSetter :: (Int -> Int) -> [Text.Text] -> (Int, Int)
readSetter applyMask [instruction, valueText] = let
    addressText = Text.init $ Text.drop 4 instruction
    address = fst $ fromRight readError $ Text.decimal addressText
    value = fst $ fromRight readError $ Text.decimal valueText
    in (address, applyMask $ rotate value (-36))

setter :: Mask -> Instruction -> (Map.Key, Int)
setter currentMask = readSetter (applyMask currentMask) . Text.splitOn instructionDelimiter

isMask :: Instruction -> Bool
isMask instruction = hasMaskPrefix $ Text.take 3 instruction

initializeMemory :: Memory
initializeMemory = Map.empty

apply :: Int -> Char -> Int
apply value 'X' = value
apply value '0' = clearBit value 0
apply value '1' = setBit value 0

applyMask :: Mask -> Int -> Int
applyMask mask value
    | Text.null mask = value
    | otherwise = applyMask (Text.tail mask) $ apply (rotate value 1) (Text.head mask)

update :: (Map.Key, Int) -> Memory -> Memory
update = uncurry Map.insert

run :: Program -> Mask -> Memory -> Memory
run program currentMask memory
    | null program = memory
    | isMask instruction = run (tail program) (mask instruction) memory
    | otherwise = run (tail program) currentMask $ update (setter currentMask instruction) memory
    where instruction = head program

calculateResult :: Memory -> Int
calculateResult = sum . fmap snd . Map.toList

loadData :: IO Program
loadData = Text.lines <$> Text.readFile "./data/data-day-14.txt"

-- Part One

programPartOne :: Program
programPartOne = [
    Text.pack "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    Text.pack "mem[8] = 11",
    Text.pack "mem[7] = 101",
    Text.pack "mem[8] = 0"
    ]

testPartOne :: Int
testPartOne = calculateResult $ run programPartOne defaultMask initializeMemory

partOne :: IO Int
partOne = do
    program <- loadData
    return $ calculateResult $ run program defaultMask initializeMemory
