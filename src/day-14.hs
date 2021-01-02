import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.IntMap.Strict as Map
import Data.Bits ( Bits(testBit, rotate, clearBit, setBit) )
import Data.Either ( fromRight )

type Program = [Text.Text]
type Memory = Map.IntMap Int
type Mask = Text.Text
type Instruction = Text.Text
type SetterOperation = (Mask -> Instruction -> [(Address, Int)])
type Address = Int

defaultMask :: Mask
defaultMask = Text.pack ""

instructionDelimiter :: Text.Text
instructionDelimiter = Text.pack " = "

readError :: (Int, Text.Text)
readError = (0, Text.pack "")

hasMaskPrefix :: Text.Text -> Bool
hasMaskPrefix = (== Text.pack "mas")

parseMask :: Instruction -> Mask
parseMask = flip (!!) 1 . Text.splitOn instructionDelimiter

parseSetter :: [Text.Text] -> (Address, Int)
parseSetter [instruction, valueText] = let
    addressText = Text.init $ Text.drop 4 instruction
    address = fst $ fromRight readError $ Text.decimal addressText
    value = fst $ fromRight readError $ Text.decimal valueText
    in (address, value)

isMask :: Instruction -> Bool
isMask instruction = hasMaskPrefix $ Text.take 3 instruction

initializeMemory :: Memory
initializeMemory = Map.empty

update :: (Address, Int) -> Memory -> Memory
update = uncurry Map.insert

run :: Program -> Mask -> SetterOperation -> Memory -> Memory
run program currentMask setter memory
    | null program = memory
    | isMask instruction = run (tail program) (parseMask instruction) setter memory
    | otherwise = run (tail program) currentMask setter $ foldl (flip update) memory (setter currentMask instruction)
    where instruction = head program

calculateResult :: Memory -> Int
calculateResult = sum . fmap snd . Map.toList

loadData :: IO Program
loadData = Text.lines <$> Text.readFile "./data/data-day-14.txt"

-- Part One

apply :: Int -> Char -> Int
apply value 'X' = value
apply value '0' = clearBit value 0
apply value '1' = setBit value 0

applyMask :: Int -> Mask -> Int
applyMask value mask
    | Text.null mask = value
    | otherwise = applyMask (apply (rotate value 1) (Text.head mask)) $ Text.tail mask

setterPartOne :: Mask -> Instruction -> [(Address, Int)]
setterPartOne currentMask instruction = let
    (address, value) = parseSetter $ Text.splitOn instructionDelimiter instruction
    in [(address, applyMask (rotate value (-36)) currentMask)]

programPartOne :: Program
programPartOne = [
    Text.pack "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    Text.pack "mem[8] = 11",
    Text.pack "mem[7] = 101",
    Text.pack "mem[8] = 0"
    ]

testPartOne :: Int
testPartOne = calculateResult $ run programPartOne defaultMask setterPartOne initializeMemory

partOne :: IO Int
partOne = do
    program <- loadData
    return $ calculateResult $ run program defaultMask setterPartOne initializeMemory

-- Part Two

replaceAt :: Text.Text -> Int -> Char -> Text.Text
replaceAt text index char = Text.append head $ Text.cons char tail
    where head = Text.take (index - 1) text
          tail = Text.drop index text

updateMask :: Mask -> Int -> Int -> Mask
updateMask mask index value
    | index == 0 = mask
    | maskBit == 'X' = updateMask (replaceAt mask index oneOrZero) (index - 1) (rotate value (-1))
    | maskBit == '0' = updateMask (replaceAt mask index 'X') (index - 1) value
    | otherwise = updateMask mask (index - 1) value
    where maskBit = Text.index mask (index - 1)
          oneOrZero = if testBit value 0 then '1' else '0'

updateAddress :: Address -> Mask -> Address
updateAddress address = applyMask (rotate address (-36))

applyMaskToAddress :: Mask -> Address -> [Address]
applyMaskToAddress mask address = updateAddress address <$> masks
    where times = Text.length $ Text.filter (== 'X') mask
          masks = updateMask mask (Text.length mask) <$> [0..((2 ^ times) - 1)]

pack :: Int -> Address -> (Address, Int)
pack value address = (address, value)

setterPartTwo :: Mask -> Instruction -> [(Address, Int)]
setterPartTwo currentMask instruction = let
    (address, value) = parseSetter $ Text.splitOn instructionDelimiter instruction
    addresses = applyMaskToAddress currentMask address
    in pack value <$> addresses

programPartTwo :: Program
programPartTwo = [
    Text.pack "mask = 000000000000000000000000000000X1001X",
    Text.pack "mem[42] = 100",
    Text.pack "mask = 00000000000000000000000000000000X0XX",
    Text.pack "mem[26] = 1"
    ]

testPartTwo :: Int
testPartTwo = calculateResult $ run programPartTwo defaultMask setterPartTwo initializeMemory

partTwo :: IO Int
partTwo = do
    program <- loadData
    return $ calculateResult $ run program defaultMask setterPartTwo initializeMemory
