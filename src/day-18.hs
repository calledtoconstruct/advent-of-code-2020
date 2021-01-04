import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char (digitToInt)

data Operation = Unknown | Add | Multiply
type Operand = Int
data Memory = Memory {
    leftOperand :: Operand,
    operation :: Operation,
    rightOperand :: Operand
}

newMemory :: Memory
newMemory = Memory { leftOperand = 0, operation = Unknown, rightOperand = 0 }

performOperation :: Memory -> Memory
performOperation memory = case operation memory of
    Add      -> memory { leftOperand = leftOperand memory + rightOperand memory, operation = Unknown, rightOperand = 0 }
    Multiply -> memory { leftOperand = leftOperand memory * rightOperand memory, operation = Unknown, rightOperand = 0 }
    _        -> memory

readOperand :: Memory -> Char -> Memory
readOperand memory digit = case operation memory of
    Unknown  -> memory { leftOperand = leftOperand memory * 10 + digitToInt digit }
    _        -> memory { rightOperand = rightOperand memory * 10 + digitToInt digit }

saveResult :: Memory -> Operand -> Memory
saveResult memory operand = case operation memory of
    Unknown  -> memory { leftOperand = operand }
    _        -> memory { rightOperand = operand }

nextOperation :: Memory -> Operation -> Memory
nextOperation memory newOperation = case operation memory of
    Unknown  -> memory { operation = newOperation }
    _        -> (performOperation memory) { operation = newOperation, rightOperand = 0 }

findMatchingParen :: Int -> Text.Text -> Text.Text
findMatchingParen count text
    | count == 0 = text
    | current == '(' = findMatchingParen (count + 1) remaining
    | current == ')' = findMatchingParen (count - 1) remaining
    | otherwise = findMatchingParen count remaining
    where current = Text.head text
          remaining = Text.tail text

evaluateExpression :: Memory -> Text.Text -> Int
evaluateExpression memory expression
    | Text.null expression = leftOperand $ performOperation memory
    | current == ')' = leftOperand $ performOperation memory
    | current == ' ' = evaluateExpression memory remaining
    | current == '+' = evaluateExpression (nextOperation memory Add) remaining
    | current == '*' = evaluateExpression (nextOperation memory Multiply) remaining
    | current == '(' = evaluateExpression (saveResult memory $ evaluateExpression newMemory remaining) $ findMatchingParen 1 remaining
    | otherwise = evaluateExpression (readOperand memory current) remaining
    where current = Text.head expression
          remaining = Text.tail expression

sampleExpressions :: [Text.Text]
sampleExpressions = [
    Text.pack "1 + 2 * 3 + 4 * 5 + 6",
    Text.pack "1 + (2 * 3) + (4 * (5 + 6))",
    Text.pack "2 * 3 + (4 * 5)",
    Text.pack "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    Text.pack "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    Text.pack "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    ]

sampleAnswers :: [Int]
sampleAnswers = [71, 51, 26, 437, 12240, 13632]

testPartOne :: [Bool]
testPartOne = uncurry (==) <$> zip (evaluateExpression newMemory <$> sampleExpressions) sampleAnswers

partOne :: IO Int
partOne = do
    expressions <- Text.lines <$> Text.readFile "./data/data-day-18.txt"
    return $ sum $ evaluateExpression newMemory <$> expressions