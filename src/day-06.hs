import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List.Unique as List (sortUniq)

data Group = Group {
    answers :: Text.Text
} deriving (Show)

collapse :: Group -> Group
collapse group = group { answers = Text.pack $ List.sortUniq $ Text.unpack $ answers group }

newGroup :: Group
newGroup = Group { answers = Text.pack "" }

appendAnswer :: Group -> Text.Text -> Group
appendAnswer group response = group { answers = flip Text.append response $ answers group }

loadAnswers :: [Group] -> [Text.Text] -> [Group]
loadAnswers groups [] = groups
loadAnswers groups responses
    | Text.null current = loadAnswers (newGroup: currentGroup: otherGroups) remaining
    | otherwise = loadAnswers (updatedGroup: otherGroups) remaining
    where updatedGroup = appendAnswer currentGroup current
          (current: remaining) = responses
          (currentGroup: otherGroups) = groups

sumAnswers :: Int -> [Group] -> Int 
sumAnswers = foldl (\ sum currentGroup -> (+) sum $ Text.length $ answers currentGroup)

sampleAnswers :: [Text.Text]
sampleAnswers = [
    Text.pack "abc",
    Text.pack "",
    Text.pack "a",
    Text.pack "b",
    Text.pack "c",
    Text.pack "",
    Text.pack "ab",
    Text.pack "ac",
    Text.pack "",
    Text.pack "a",
    Text.pack "a",
    Text.pack "a",
    Text.pack "a",
    Text.pack "",
    Text.pack "b"
    ]

loadData :: IO [Text.Text]
loadData = Text.lines <$> Text.readFile "./data/data-day-06.txt"

partOne :: IO Int 
partOne = do
    responses <- loadData
    return $ sumAnswers 0 $ collapse <$> loadAnswers [newGroup] responses