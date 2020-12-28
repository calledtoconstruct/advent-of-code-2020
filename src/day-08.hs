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

newInstruction :: Text.Text -> Int -> Instruction
newInstruction code' parameter' = Instruction { code = code', parameter = parameter', executed = False }

-- nop +0
-- acc +1
-- jmp +4
-- acc +3
-- jmp -3
-- acc -99
-- acc +1
-- jmp -4
-- acc +6

nop :: Instruction -> Registers -> Registers
nop _ (rax, rip) = (rax, rip + 1)

acc :: Instruction -> Registers -> Registers
acc instruction (rax, rip) = (rax + parameter instruction, rip + 1)

jmp :: Instruction -> Registers -> Registers
jmp instruction (rax, rip) = (rax, rip + parameter instruction)

step :: Program -> Registers -> Int
step program (rax, rip)
    | length program <= rip = rax
    | executed current = rax
    | code current == Text.pack "nop" = step updated $ nop current (rax, rip)
    | code current == Text.pack "acc" = step updated $ acc current (rax, rip)
    | code current == Text.pack "jmp" = step updated $ jmp current (rax, rip)
    | otherwise = rax
    where current = program !! max 0 rip
          executed' = current { executed = True }
          updated = take rip program ++ [executed'] ++ drop 1 (drop rip program)

execute :: Program -> Int
execute program = step program (0, 0)

executeSample :: Int
executeSample = execute sampleProgram

readInstruction :: Text.Text -> Instruction
readInstruction instructionText = newInstruction code parameter
    where tokens = Text.split (== ' ') instructionText
          code = head tokens
          parameter = fst $ fromRight (0, Text.pack "") $ Text.signed Text.decimal $ last tokens

loadProgram :: IO Program
loadProgram = do
    instructionText <- Text.lines <$> Text.readFile "./data/data-day-08.txt"
    return $ readInstruction <$> instructionText
    
executeProgram :: IO Int
executeProgram = do execute <$> loadProgram
