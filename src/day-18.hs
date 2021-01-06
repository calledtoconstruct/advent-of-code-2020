import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char (digitToInt)
import Data.Foldable
import qualified Data.Bifunctor
import Data.Bifunctor (Bifunctor(bimap))

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
saveResult memory value = case operation memory of
    Unknown  -> memory { leftOperand = value }
    _        -> memory { rightOperand = value }

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

data LookingFor = LeftOperand | EndOfLeftOperand | Plus | RightOperand | EndOfRightOperand deriving (Eq)
data OperandType = Number | Expression | NotFound

operandAt :: Text.Text -> Int -> Maybe OperandType
operandAt expression at
    | current `elem` ['0'..'9'] = Just Number
    | current == '(' = Just Expression
    | otherwise = Nothing
    where current = Text.index expression at

reprioritize :: LookingFor -> Int -> Int -> Text.Text -> (Int, Text.Text)
reprioritize LeftOperand positionOfLeftOperand positionOfEvaluation expression = case operandAt expression positionOfEvaluation of
    Just Number     -> reprioritize EndOfLeftOperand positionOfEvaluation (positionOfEvaluation + 1) expression
    Just Expression -> uncurry (reprioritize EndOfLeftOperand positionOfEvaluation) recursed
    Nothing         -> reprioritize LeftOperand positionOfLeftOperand (positionOfEvaluation + 1) expression
    where recursed = reprioritize LeftOperand (positionOfEvaluation + 1) (positionOfEvaluation + 1) expression

reprioritize EndOfLeftOperand positionOfLeftOperand positionOfEvaluation expression = if positionOfEvaluation < Text.length expression
    then case Text.index expression positionOfEvaluation of
        ' ' -> uncurry (reprioritize Plus positionOfLeftOperand) result
        ')' -> result
    else result
    where result = (positionOfEvaluation + 1, expression)

reprioritize Plus positionOfLeftOperand positionOfEvaluation expression = case Text.index expression positionOfEvaluation of
    '+' -> reprioritize RightOperand positionOfLeftOperand (positionOfEvaluation + 1) expression
    '*' -> reprioritize LeftOperand positionOfEvaluation (positionOfEvaluation + 1) expression

reprioritize RightOperand positionOfLeftOperand positionOfEvaluation expression = case operandAt expression positionOfEvaluation of
    Just Number     -> reprioritize EndOfRightOperand positionOfLeftOperand (positionOfEvaluation + 1) expression
    Just Expression -> uncurry (reprioritize EndOfRightOperand positionOfLeftOperand) recursed
    Nothing         -> reprioritize RightOperand positionOfLeftOperand (positionOfEvaluation + 1) expression
    where recursed = reprioritize LeftOperand (positionOfEvaluation + 1) (positionOfEvaluation + 1) expression

reprioritize EndOfRightOperand positionOfLeftOperand positionOfEvaluation expression = if positionOfEvaluation < Text.length expression
    then case Text.index expression positionOfEvaluation of
        ' ' -> uncurry (reprioritize Plus positionOfLeftOperand) result
        ')' -> result
    else result
    where result = (positionOfEvaluation + 3, prioritize expression positionOfLeftOperand positionOfEvaluation)

prioritize :: Text.Text -> Int -> Int -> Text.Text
prioritize expression begin end = Text.concat [Text.take begin expression, prioritized, Text.drop end expression]
    where prioritized = Text.concat [Text.pack "(", Text.take (end - begin) $ Text.drop begin expression, Text.pack ")"]

testPartTwo :: [Bool]
testPartTwo = [
    (==) (Text.pack "(8 * (2 + 4))") $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "(8 * 2 + 4)",
    (==) (Text.pack "5 * ((((5 + 6) * 6) + (3 * 7 * (6 + 9) * (4 + 9))) + 9)") $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "5 * (5 + 6 * 6) + (3 * 7 * 6 + 9 * 4 + 9) + 9",
    (==) 231 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "1 + 2 * 3 + 4 * 5 + 6",
    (==) 51 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "1 + (2 * 3) + (4 * (5 + 6))",
    (==) 46 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "2 * 3 + (4 * 5)",
    (==) 1445 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    (==) 669060 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    (==) 23340 $ evaluateExpression newMemory $ snd $ reprioritize LeftOperand 0 0 $ Text.pack "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    ]

data X = X {
    a :: String,
    b :: String
} deriving (Show)

printPartTwo :: IO ()
printPartTwo = do
    expressions <- Text.lines <$> Text.readFile "./data/data-day-18.txt"
    let reprioritized = (\ expr -> X { a = Text.unpack $ snd $ reprioritize LeftOperand 0 0 expr, b = Text.unpack expr}) <$> expressions
    mapM_ print reprioritized

partTwo :: IO Int
partTwo = do
    expressions <- Text.lines <$> Text.readFile "./data/data-day-18.txt"
    return $ sum $ evaluateExpression newMemory . snd . reprioritize LeftOperand 0 0 <$> expressions
