import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List (sort)
import qualified Data.Either as Text

type Index = Int
type Preamble = Int
type Program = [Int]
type Segment = [Int]
type Error = Int
type Range = (Int, Int)

validate :: Segment -> Int -> Bool
validate segment target
    | length segment < 2 = False
    | upper + lower > target = validate (init segment) target
    | upper + lower < target = validate (tail segment) target
    | otherwise = True
    where lower = head segment
          upper = last segment

findCorrupt :: Program -> Preamble -> Error
findCorrupt program preamble
    | valid = findCorrupt (tail program) preamble
    | otherwise = target
    where segment = sort $ take preamble program
          target = program !! max 0 preamble
          valid = validate segment target

loadProgram :: IO Program
loadProgram = do 
    programSource <- Text.lines <$> Text.readFile "./data/data-day-09.txt"
    return $ fst . Text.fromRight (0, Text.pack "") . Text.decimal <$> programSource

-- Part One

samplePartOne :: Program
samplePartOne = [35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576]

partOneSample :: Error
partOneSample = findCorrupt samplePartOne 5

partOne :: IO Error
partOne = do
    program <- loadProgram
    return $ findCorrupt program 25

-- Part Two

findWeakness :: Program -> Range -> Int -> Int
findWeakness program (lower, upper) target
    | sum' < target = findWeakness program (lower, upper + 1) target
    | sum' > target = findWeakness program (lower + 1, upper) target
    | otherwise = least + most
    where segment = take (upper - lower) $ drop lower program
          sum' = sum segment
          sorted = sort segment
          least = head sorted
          most = last sorted

partTwo :: IO Error
partTwo = do
    program <- loadProgram
    let corrupt = findCorrupt program 25
    return $ findWeakness program (0, 1) corrupt
    