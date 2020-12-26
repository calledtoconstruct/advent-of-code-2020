import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either

data PasswordCheck = PasswordCheck {
    minimumOccurrance :: Int,
    maximumOccurrance :: Int,
    character :: Char,
    password :: Text.Text
} deriving (Show)

-- Part One

exampleData :: [PasswordCheck]
exampleData = [
    PasswordCheck { minimumOccurrance=1, maximumOccurrance=3, character='a', password=Text.pack "abcde" },
    PasswordCheck { minimumOccurrance=1, maximumOccurrance=3, character='b', password=Text.pack "cdefg" },
    PasswordCheck { minimumOccurrance=2, maximumOccurrance=9, character='c', password=Text.pack "ccccccccc" }
    ]

-- Solution

checkPolicy :: PasswordCheck -> Bool 
checkPolicy passwordCheck = min <= occurrances && occurrances <= max
    where psw = password passwordCheck
          min = minimumOccurrance passwordCheck
          max = maximumOccurrance passwordCheck
          ch = character passwordCheck
          occurrances = Text.length $ Text.filter (== ch) psw

count :: [PasswordCheck] -> (Int, Int, Int)
count passwordChecks = (length valid, length invalid, length passwordChecks)
    where valid = filter checkPolicy passwordChecks
          invalid = filter (not . checkPolicy) passwordChecks

readCheck :: Text.Text -> PasswordCheck
readCheck input = PasswordCheck {
    minimumOccurrance = fst $ fromRight (0, Text.pack "") min,
    maximumOccurrance = fst $ fromRight (0, Text.pack "") max,
    character = ch,
    password = pass
    }
    where splitDash = Text.splitOn (Text.pack "-") input
          splitSpace = Text.splitOn (Text.pack " ") $ last splitDash
          min = Text.decimal $ head splitDash
          max = Text.decimal $ head splitSpace
          ch = Text.head $ head $ tail splitSpace
          pass = last splitSpace

readActualPasswordChecks :: IO [PasswordCheck]
readActualPasswordChecks = do
    passwordCheckText <- fmap Text.lines (Text.readFile "data-day-02.txt")
    return $ readCheck <$> passwordCheckText
