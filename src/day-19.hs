import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either (fromRight)
import Data.Maybe (isNothing)

data Rule = Rule {
    index :: Index,
    left :: Either [Int] Char,
    right :: Maybe [Int]
} deriving (Show)

type Puzzle = ([Rule], [Text.Text])
type Index = Int

readValues :: Text.Text -> [Int]
readValues input = map (fst . fromRight (0, Text.pack "") . Text.decimal) $ filter (not . Text.null) $ Text.splitOn (Text.pack " ") input

readValue :: Text.Text -> Char
readValue input = Text.index input 2

readRule :: Text.Text -> Rule
readRule input
    | Text.last values == '"' = Rule { index = index, left = Right value, right = Nothing }
    | isNothing $ Text.findIndex ('|' ==) values = Rule { index = index, left = Left leftValues, right = Nothing }
    | otherwise = Rule { index = index, left = Left leftValues, right = Just rightValues }
    where [indexText, values] = Text.splitOn (Text.pack ":") input
          leftAndRight = Text.splitOn (Text.pack "|") values
          index = fst $ fromRight (0, Text.pack "") $ Text.decimal indexText
          leftValues = readValues $ head leftAndRight
          rightValues = readValues $ last leftAndRight
          value = readValue values

findRule :: [Rule] -> Index -> Rule
findRule rules rule = head $ filter ((==) rule . index) rules

crossJoin :: [[Text.Text]] -> [Text.Text]
crossJoin input
    | length input == 2 = [ Text.append x y | x <- head input, y <- input !! 1]
    | length input > 2 = crossJoin ([ Text.append x y | x <- head input, y <- input !! 1] : drop 2 input)
    | otherwise = head input

leftSequence :: [Rule] -> Rule -> [Text.Text]
leftSequence rules currentRule = case left currentRule of
    Right value -> [Text.singleton value]
    Left leftValues -> let
        leftRules = findRule rules <$> leftValues
        in crossJoin $ calculateRule rules <$> leftRules

rightSequence :: [Rule] -> Rule -> [Text.Text]
rightSequence rules currentRule = case right currentRule of
    Just rightValues -> let
        rightRules = findRule rules <$> rightValues
        in crossJoin $ calculateRule rules <$> rightRules
    Nothing -> []

calculateRule :: [Rule] -> Rule -> [Text.Text]
calculateRule rules currentRule = ls ++ rs
    where ls = leftSequence rules currentRule
          rs = rightSequence rules currentRule

-- aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, or ababbb

sampleRules :: [Rule]
sampleRules = [
    Rule { index = 0, left = Left [4, 1, 5], right = Nothing },
    Rule { index = 1, left = Left [2, 3], right = Just [3, 2] },
    Rule { index = 2, left = Left [4, 4], right = Just [5, 5] },
    Rule { index = 3, left = Left [4, 5], right = Just [5, 4] },
    Rule { index = 4, left = Right 'a', right = Nothing },
    Rule { index = 5, left = Right 'b', right = Nothing }
    ]

loadData :: IO Puzzle
loadData = do
    lines <- Text.lines <$> Text.readFile "./data/data-day-19.txt"
    let ruleText = takeWhile (not . Text.null) lines
    let codes = tail $ dropWhile (not . Text.null) lines
    let rules = readRule <$> ruleText
    return (rules, codes)

partOne :: IO Int
partOne = do
    (rules, codes) <- loadData
    let allowed = calculateRule rules $ findRule rules 0
    return $ length $ filter (`elem` allowed) codes
