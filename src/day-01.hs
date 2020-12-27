import Data.Maybe
import Data.Sort
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- Part One

example :: [Int]
example = [1721, 979, 366, 299, 675, 1456]

partOneExampleAnswer :: Int
partOneExampleAnswer = 514579

testPartOne :: [Int] -> Int -> Bool
testPartOne expenses answer = product == answer
    where product = multiplyTwoExpenses found
          found = fromMaybe (0, 0) $ findTwoExpensesThatSumToTwentyTwenty sorted
          sorted = sort expenses

-- Solution

actualExpenses :: IO [Int]
actualExpenses = do
    expenses <- fmap Text.lines (Text.readFile "./data/data-day-01.txt")
    return $ fmap (read . Text.unpack) expenses

findTwoExpensesThatSumToTwentyTwenty :: [Int] -> Maybe (Int, Int)
findTwoExpensesThatSumToTwentyTwenty [] = Nothing
findTwoExpensesThatSumToTwentyTwenty [expense] = Nothing
findTwoExpensesThatSumToTwentyTwenty expenses
    | sum == 2020 = Just (first, second)
    | sum > 2020 = findTwoExpensesThatSumToTwentyTwenty $ init expenses
    | otherwise = findTwoExpensesThatSumToTwentyTwenty $ tail expenses
    where first = head expenses
          second = last expenses
          sum = first + second

multiplyTwoExpenses :: (Int, Int) -> Int
multiplyTwoExpenses (a, b) = answer
    where answer = a * b

partOneAnswer :: IO Int 
partOneAnswer = do
    expenses <- actualExpenses
    let found = fromMaybe (0, 0) $ findTwoExpensesThatSumToTwentyTwenty $ sort expenses 
    return $ multiplyTwoExpenses found

-- Part Two

partTwoExampleAnswer :: Int 
partTwoExampleAnswer = 241861950

testPartTwo :: [Int] -> Int -> Bool
testPartTwo expenses answer = product == answer
    where product = multiplyThreeExpenses found
          found = fromMaybe (0, 0, 0) $ findThreeExpensesThatSumToTwentyTwenty sorted 1
          sorted = sort expenses

multiplyThreeExpenses :: (Int, Int, Int) -> Int
multiplyThreeExpenses (a, b, c) = answer
    where answer = a * b * c

findThreeExpensesThatSumToTwentyTwenty :: [Int] -> Int -> Maybe (Int, Int, Int)
findThreeExpensesThatSumToTwentyTwenty [] _ = Nothing
findThreeExpensesThatSumToTwentyTwenty [expense] _ = Nothing
findThreeExpensesThatSumToTwentyTwenty expenses middlePosition
    | right >= 2020 || left + right >= 2020 || left + middle + right > 2020 = findThreeExpensesThatSumToTwentyTwenty (init expenses) 1
    | left + middle + right == 2020 = Just (left, middle, right)
    | middlePosition + 1 < length expenses = findThreeExpensesThatSumToTwentyTwenty expenses (middlePosition + 1)
    | otherwise = findThreeExpensesThatSumToTwentyTwenty (tail expenses) 1
    where left = head expenses
          right = last expenses
          middle = last $ take middlePosition expenses

partTwoAnswer :: IO Int
partTwoAnswer = do
    expenses <- actualExpenses
    let found = fromMaybe (0, 0, 0) $ findThreeExpensesThatSumToTwentyTwenty (sort expenses) 1
    return $ multiplyThreeExpenses found
