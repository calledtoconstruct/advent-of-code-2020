import Data.Maybe
import Data.Sort
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

example :: [Integer]
example = [1721, 979, 366, 299, 675, 1456]

exampleAnswer :: Integer
exampleAnswer = 514579

actualExpenses :: IO [Integer]
actualExpenses = do
    expenses <- fmap Text.lines (Text.readFile "day-01.txt")
    return $ fmap (read . Text.unpack) expenses

findExpensesThatSumToTwentyTwenty :: [Integer] -> Maybe (Integer, Integer)
findExpensesThatSumToTwentyTwenty [] = Nothing
findExpensesThatSumToTwentyTwenty [expense] = Nothing
findExpensesThatSumToTwentyTwenty expenses
    | sum == 2020 = Just (first, second)
    | sum > 2020 = findExpensesThatSumToTwentyTwenty $ init expenses
    | otherwise = findExpensesThatSumToTwentyTwenty $ tail expenses
    where first = head expenses
          second = last expenses
          sum = first + second

multiplyExpenses :: (Integer, Integer) -> Integer
multiplyExpenses (a, b) = answer
    where answer = a * b

actualAnswer :: IO Integer 
actualAnswer = do
    expenses <- actualExpenses
    let found = fromMaybe (0, 0) $ findExpensesThatSumToTwentyTwenty $ sort expenses 
    return $ multiplyExpenses found

test :: [Integer] -> Integer -> Bool
test expenses answer = product == answer
    where product = multiplyExpenses found
          found = fromMaybe (0, 0) $ findExpensesThatSumToTwentyTwenty sorted
          sorted = sort expenses