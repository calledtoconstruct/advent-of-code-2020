import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Either (fromRight)
import qualified Data.Text.Read as Text

data Instruction = Instruction {
    code :: Text.Text,
    parameter :: Int,
    executed :: Bool
} deriving (Show)

type Program = [Instruction]

type Registers = (Int, Int)

type Result = (Bool, Int)

newInstruction :: Text.Text -> Int -> Instruction
newInstruction code' parameter' = Instruction { code = code', parameter = parameter', executed = False }

nop :: Instruction -> Registers -> Registers
nop _ (rax, rip) = (rax, rip + 1)

acc :: Instruction -> Registers -> Registers
acc instruction (rax, rip) = (rax + parameter instruction, rip + 1)

jmp :: Instruction -> Registers -> Registers
jmp instruction (rax, rip) = (rax, rip + parameter instruction)

step :: Int -> Program -> Registers -> Result
step repair program (rax, rip)
    | length program <= rip = (True, rax)
    | executed current = (False, rax)
    | code current == Text.pack "acc" = resultAcc
    | code current == Text.pack "nop" && repair == 0 = resultNop
    | code current == Text.pack "jmp" && repair == 0 = resultJmp
    | code current == Text.pack "nop" && fst resultNop = resultNop
    | code current == Text.pack "jmp" && fst resultJmp = resultJmp
    | code current == Text.pack "nop" && repair > 0 && fst resultRepairJmp = resultRepairJmp
    | code current == Text.pack "jmp" && repair > 0 && fst resultRepairNop = resultRepairNop
    | otherwise = (False, rax)
    where current = program !! max 0 rip
          executed' = current { executed = True }
          updated = take rip program ++ [executed'] ++ drop 1 (drop rip program)
          resultAcc = step repair updated $ acc current (rax, rip)
          resultNop = step repair updated $ nop current (rax, rip)
          resultJmp = step repair updated $ jmp current (rax, rip)
          resultRepairNop = step (repair - 1) updated $ nop current (rax, rip)
          resultRepairJmp = step (repair - 1) updated $ jmp current (rax, rip)

execute :: Int -> Program -> Result
execute repair program = step repair program (0, 0)

readInstruction :: Text.Text -> Instruction
readInstruction instructionText = newInstruction code parameter
    where tokens = Text.split (== ' ') instructionText
          code = head tokens
          parameter = fst $ fromRight (0, Text.pack "") $ Text.signed Text.decimal $ last tokens

loadProgram :: IO Program
loadProgram = do
    instructionText <- Text.lines <$> Text.readFile "./data/data-day-08.txt"
    return $ readInstruction <$> instructionText

-- Part One

sampleProgram :: Program
sampleProgram = [
    newInstruction (Text.pack "nop") 0,
    newInstruction (Text.pack "acc") 1,
    newInstruction (Text.pack "jmp") 4,
    newInstruction (Text.pack "acc") 3,
    newInstruction (Text.pack "jmp") (-3),
    newInstruction (Text.pack "acc") (-99),
    newInstruction (Text.pack "acc") 1,
    newInstruction (Text.pack "jmp") (-4),
    newInstruction (Text.pack "acc") 6
    ]

-- nop +0
-- acc +1
-- jmp +4
-- acc +3
-- jmp -3
-- acc -99
-- acc +1
-- jmp -4
-- acc +6

executeSamplePartOne :: Result
executeSamplePartOne = execute 0 sampleProgram

executePartOne :: IO Result
executePartOne = do execute 0 <$> loadProgram

-- Part Two

executeSamplePartTwo :: Result
executeSamplePartTwo = execute 1 sampleProgram

executePartTwo :: IO Result
executePartTwo = do execute 1 <$> loadProgram
