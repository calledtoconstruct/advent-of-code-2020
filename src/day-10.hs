import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List (sort)
import qualified Data.Either as Text
import Data.List.Extra ( snoc )

type StepCount = (Int, Int)

countSteps :: StepCount -> [Int] -> StepCount
countSteps (countOneStep, countThreeStep) adapters
    | length adapters == 1 = (countOneStep, countThreeStep + 1)
    | step == 1 = countSteps (countOneStep + 1, countThreeStep) (tail adapters)
    | step == 3 = countSteps (countOneStep, countThreeStep + 1) (tail adapters)
    where lower = head adapters
          upper = adapters !! 1
          step = upper - lower

loadData :: IO [Int]
loadData = do 
    dataSource <- Text.lines <$> Text.readFile "./data/data-day-10.txt"
    return $ fst . Text.fromRight (0, Text.pack "") . Text.decimal <$> dataSource

-- Part One

smallSampleData :: [Int]
smallSampleData = [16
    ,10
    ,15
    ,5
    ,1
    ,11
    ,7
    ,19
    ,6
    ,12
    ,4]

sampleData :: [Int]
sampleData = [28
    ,33
    ,18
    ,42
    ,31
    ,14
    ,46
    ,20
    ,48
    ,47
    ,24
    ,23
    ,49
    ,45
    ,19
    ,38
    ,39
    ,11
    ,1
    ,32
    ,25
    ,35
    ,8
    ,17
    ,7
    ,9
    ,4
    ,2
    ,34
    ,10
    ,3]

partOneSample :: StepCount
partOneSample = countSteps (0, 0) $ sort $ 0: sampleData

partOne :: IO Int
partOne = do
    adapters <- loadData
    let (oneStep, threeStep) = countSteps (0, 0) $ sort $ 0: adapters
    return $ oneStep * threeStep

-- Part Two

-- 3, 1, 1, 1, 1, 1
-- 3, 4, 5, 6, 7, 8

-- 345678
-- 3 5678
-- 34 678
-- 345 78
-- 3456 8
-- 3  678
-- 34  78
-- 345  8
-- 3 5 78
-- 34 6 8
-- 3  6 8
-- 3 5  8

-- 7x1 = 40    18
-- 6x1 = 22 ?  10
-- 5x1 = 12  6
-- 4x1 = 6  2
-- 3x1 = 4  2
-- 2x1 = 2  1

-- 1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3 -> 8 permutations

-- ###########
-- ## ########
-- ### #######
-- ##  #######
-- ###### ####
-- ## ### ####
-- ### ## ####
-- ##  ## ####

-- (3), 1, 3, 1, (3) -> [1, 3, 1] -> 1 permutations
-- (0), 1, 4, 5, (8) -> [1, 4, 5]
-- (3), 1, 1, 3, 3, 1, 1, (3) -> 4 permutations
-- (0), 1, 2, 5, 8, 9, 10, (13) -> [1, 2, 5, 8, 9, 10], [2, 5, 8, 9, 10], [1, 2, 5, 8, 10], [2, 5, 8, 10]
-- (?), 3, 1, 1, 3, 1, 1, (3) -> [3, 4, 5, 8, 9, 10], [3, 5, 8, 9, 10], [3, 4, 5, 8, 10], [3, 5, 8, 10]
-- (0), 3, 4, 5, 8, 9, 10, (13) -> 03, !04, 34, 35, !38, 58, !59, 89, 810, !813, 1013 = 

-- 3458910
-- 3 58910
-- 3458 10
-- 3 58 10

-- (?), 1, 1, 1, 1, 3, (?) -> 6
-- (0), 1, 2, 3, 4, 7, (10) -> 01, 02, 03, !04, 14, !17, !27, !37, 47, !410, 710 = 6

-- 12347
--  2347
-- 1 347
--   347
--  2 47
-- 1  47

multiplier :: Int -> [Int] -> Int
multiplier count current
    | count == 0 = last current
    | otherwise = multiplier (count - 1) $ next: init current
    where next = sum current

maxSequenceOfOnes :: Int -> Int -> [Int] -> Int
maxSequenceOfOnes best streak values
    | null values = max best streak
    | head values == 1 = maxSequenceOfOnes best (streak + 1) $ tail values
    | otherwise = maxSequenceOfOnes (max best streak) 0 $ tail values

calculateOffsets :: [Int] -> [Int] -> [Int]
calculateOffsets adapters offsets
    | length adapters == 1 = offsets
    | otherwise = calculateOffsets (tail adapters) $ snoc offsets offset
    where offset = (adapters !! 1) - head adapters

calculatePermutations :: [Int] -> Int -> Int -> Int
calculatePermutations offsets permutations streak
    | null offsets && streak > 1        = nextPermutations
    | null offsets                      = permutations
    | head offsets == 1                 = calculatePermutations nextOffsets permutations nextStreak
    | head offsets == 3 && streak > 1   = calculatePermutations nextOffsets nextPermutations 0
    | otherwise                         = calculatePermutations nextOffsets permutations 0
    where multiplyBy = multiplier (streak - 2) [7, 4, 2]
          nextOffsets = tail offsets
          nextPermutations = permutations * multiplyBy
          nextStreak = streak + 1

partTwo :: IO Int
partTwo = do
    adapters <- loadData
    return $ calculatePermutations (calculateOffsets (sort (0: adapters)) []) 1 0