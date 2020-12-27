
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data KeyValuePair = KeyValuePair {
    key :: Text.Text,
    value :: Text.Text
} deriving (Show)

getKeys :: [KeyValuePair] -> [Text.Text]
getKeys entries = key <$> entries

data Individual = Individual {
    entries :: [KeyValuePair]
} deriving (Show)

sampleDataPartOne :: [Text.Text]
sampleDataPartOne = [
    Text.pack "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
    Text.pack "byr:1937 iyr:2017 cid:147 hgt:183cm",
    Text.pack "",
    Text.pack "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
    Text.pack "hcl:#cfa07d byr:1929",
    Text.pack "",
    Text.pack "hcl:#ae17e1 iyr:2013",
    Text.pack "eyr:2024",
    Text.pack "ecl:brn pid:760753108 byr:1931",
    Text.pack "hgt:179cm",
    Text.pack "",
    Text.pack "hcl:#cfa07d eyr:2025 pid:166559648",
    Text.pack "iyr:2011 ecl:brn hgt:59in"
    ]

requiredKeys :: [Text.Text]
requiredKeys = [
    Text.pack "byr",
    Text.pack "iyr",
    Text.pack "eyr",
    Text.pack "hgt",
    Text.pack "hcl",
    Text.pack "ecl",
    Text.pack "pid"
    ]

newIndividual :: Individual
newIndividual = Individual { entries = [] }

loadData :: [Text.Text] -> [Individual] -> Individual -> [Individual]
loadData [] complete finalIndividual = finalIndividual: complete
loadData pendingInput complete currentIndividual
    | Text.null currentInput = loadData remainingInput (currentIndividual: complete) newIndividual
    | otherwise = loadData remainingInput complete ammendedIndividual
    where (currentInput: remainingInput) = pendingInput
          splitKeyValuePairs = Text.splitOn (Text.pack " ") currentInput
          splitKeysAndValues = Text.splitOn (Text.pack ":") <$> splitKeyValuePairs
          keyValuePairs = (\[k, v] -> KeyValuePair { key = k, value = v }) <$> splitKeysAndValues
          ammendedIndividual = currentIndividual { entries = entries currentIndividual ++ keyValuePairs }

validateIndividual :: Individual -> Bool 
validateIndividual individual = length filtered == length requiredKeys
    where individualKeys = getKeys $ entries individual
          filtered = filter (`elem` requiredKeys) individualKeys

countValidIndividuals :: Int
countValidIndividuals = length $ filter validateIndividual $ loadData sampleDataPartOne [] newIndividual

readActual :: IO [Text.Text]
readActual = Text.lines <$> Text.readFile "data-day-04.txt"

countActualValidIndividuals :: IO Int
countActualValidIndividuals = do
    actual <- readActual
    return $ length $ filter validateIndividual $ loadData actual [] newIndividual
    