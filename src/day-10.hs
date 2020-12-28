import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List (sort)
import qualified Data.Either as Text

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
