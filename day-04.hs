import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either ( fromRight )

data KeyValuePair = KeyValuePair {
    key :: Text.Text,
    value :: Text.Text
} deriving (Show)

getKeys :: [KeyValuePair] -> [Text.Text]
getKeys entries = key <$> entries

data Individual = Individual {
    entries :: [KeyValuePair]
} deriving (Show)

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

-- Part One

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

validateIndividual :: Individual -> Bool 
validateIndividual individual = length filtered == length requiredKeys
    where individualKeys = getKeys $ entries individual
          filtered = filter (`elem` requiredKeys) individualKeys

countValidIndividuals :: Int
countValidIndividuals = length $ filter validateIndividual $ loadData sampleDataPartOne [] newIndividual

countActualValidIndividuals :: IO Int
countActualValidIndividuals = do
    actual <- readActual
    return $ length $ filter validateIndividual $ loadData actual [] newIndividual

readActual :: IO [Text.Text]
readActual = Text.lines <$> Text.readFile "data-day-04.txt"
    
-- Part Two

getValue :: Individual -> Text.Text -> Text.Text
getValue individual nameOfKey = value $ head $ filter (\kvp -> key kvp == nameOfKey) $ entries individual

getCount :: Individual -> Text.Text -> Int
getCount individual nameOfKey = length $ filter (\kvp -> key kvp == nameOfKey) $ entries individual

hexCharacters :: [Char]
hexCharacters = digitCharacters ++ ['a', 'b', 'c', 'd', 'e', 'f']

digitCharacters :: [Char]
digitCharacters = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

eyeColors :: [Text.Text]
eyeColors = [
    Text.pack "amb",
    Text.pack "blu",
    Text.pack "brn",
    Text.pack "gry",
    Text.pack "grn",
    Text.pack "hzl",
    Text.pack "oth"]

validateRules :: Individual -> Bool
validateRules individual
    | not $ validateIndividual individual = False
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
    | byrCount /= 1 = False 
    | Text.length byrValue /= 4 = False
    | Text.length byrFiltered /= 4 = False
    | 1920 > byrDecimalValue || byrDecimalValue > 2002 = False
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    | iyrCount /= 1 = False 
    | Text.length iyrValue /= 4 = False
    | Text.length iyrFiltered /= 4 = False
    | 2010 > iyrDecimalValue || iyrDecimalValue > 2020 = False
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    | eyrCount /= 1 = False 
    | Text.length eyrValue /= 4 = False
    | Text.length eyrFiltered /= 4 = False
    | 2020 > eyrDecimalValue || eyrDecimalValue > 2030 = False
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
    | hgtCount /= 1 = False
    | hgtCentimeters && (150 > hgtDecimalValue || hgtDecimalValue > 193) = False
    | hgtCentimeters && (Text.length hgtValue /= 5) = False
    | hgtInches && (59 > hgtDecimalValue || hgtDecimalValue > 76) = False
    | hgtInches && (Text.length hgtValue /= 4) = False
    | not hgtCentimeters && not hgtInches = False
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    | hclCount /= 1 = False
    | hclPrefix /= '#' = False
    | Text.length hclRemaining /= 6 = False
    | Text.length hclFiltered /= 6 = False
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    | (/=) 1 $ length $ filter (== eclValue) eyeColors = False
    | eclCount /= 1 = False 
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
    | pidCount /= 1 = False
    | Text.length pidValue /= 9 = False
    | Text.length pidFiltered /= 9 = False
    | otherwise = True
    where byrValue = getValue individual (Text.pack "byr")
          byrCount = getCount individual (Text.pack "byr")
          byrFiltered = Text.filter (`elem` digitCharacters) byrValue
          byrDecimalValue = fst $ fromRight (0, Text.pack "") $ Text.decimal byrValue
          iyrValue = getValue individual (Text.pack "iyr")
          iyrCount = getCount individual (Text.pack "iyr")
          iyrFiltered = Text.filter (`elem` digitCharacters) iyrValue
          iyrDecimalValue = fst $ fromRight (0, Text.pack "") $ Text.decimal iyrValue
          eyrValue = getValue individual (Text.pack "eyr")
          eyrCount = getCount individual (Text.pack "eyr")
          eyrFiltered = Text.filter (`elem` digitCharacters) eyrValue
          eyrDecimalValue = fst $ fromRight (0, Text.pack "") $ Text.decimal eyrValue
          hgtValue = getValue individual (Text.pack "hgt")
          hgtCount = getCount individual (Text.pack "hgt")
          hgtSuffix = Text.take 2 $ Text.reverse hgtValue
          hgtCentimeters = hgtSuffix == Text.pack "mc"
          hgtInches = hgtSuffix == Text.pack "ni"
          hgtDecimalValue = fst $ fromRight (0, Text.pack "") $ Text.decimal hgtValue
          hclValue = getValue individual (Text.pack "hcl")
          hclCount = getCount individual (Text.pack "hcl")
          hclPrefix = Text.head hclValue
          hclRemaining = Text.tail hclValue
          hclFiltered = Text.filter (`elem` hexCharacters) hclRemaining
          eclValue = getValue individual (Text.pack "ecl")
          eclCount = getCount individual (Text.pack "ecl")
          pidValue = getValue individual (Text.pack "pid")
          pidCount = getCount individual (Text.pack "pid")
          pidFiltered = Text.filter (`elem` digitCharacters) pidValue

invalidSamples :: [Individual]
invalidSamples = loadData [
    Text.pack "eyr:1972 cid:100",
    Text.pack "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    Text.pack "",
    Text.pack "iyr:2019",
    Text.pack "hcl:#602927 eyr:1967 hgt:170cm",
    Text.pack "ecl:grn pid:012533040 byr:1946",
    Text.pack "",
    Text.pack "hcl:dab227 iyr:2012",
    Text.pack "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    Text.pack "",
    Text.pack "hgt:59cm ecl:zzz",
    Text.pack "eyr:2038 hcl:74454a iyr:2023",
    Text.pack "pid:3556412378 byr:2007"] [] newIndividual

countInvalidSamples :: Int
countInvalidSamples = length $ filter (not . validateRules) invalidSamples

validSamples :: [Individual]
validSamples = loadData [
    Text.pack "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
    Text.pack "hcl:#623a2f",
    Text.pack "",
    Text.pack "eyr:2029 ecl:blu cid:129 byr:1989",
    Text.pack "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
    Text.pack "",
    Text.pack "hcl:#888785",
    Text.pack "hgt:164cm byr:2001 iyr:2015 cid:88",
    Text.pack "pid:545766238 ecl:hzl",
    Text.pack "eyr:2022",
    Text.pack "",
    Text.pack "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    ] [] newIndividual

countValidSamples :: Int
countValidSamples = length $ filter validateRules validSamples

countValidRules :: Int
countValidRules = length $ filter validateRules $ loadData sampleDataPartOne [] newIndividual

countActualValidRules :: IO Int
countActualValidRules = do
    actual <- readActual
    return $ length $ filter validateRules $ loadData actual [] newIndividual
