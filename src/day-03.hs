import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- Part One

examplePartOne :: [[Char]]
examplePartOne = [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
    ]

exampleAnswerPartOne :: Int
exampleAnswerPartOne = 7

width :: [[Char]] -> Int 
width board = length $ head board

step :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
step (currentX, currentY) (stepX, stepY) maximumX = ((currentX + stepX) `mod` maximumX, currentY + stepY)

getRow :: Int -> [[Char]] -> [Char]
getRow rowNumber = last . take (rowNumber + 1)

getColumn :: Int -> [Char] -> Char
getColumn columnNumber = last . take (columnNumber + 1)

get :: (Int, Int) -> [[Char]] -> Char
get (currentX, currentY) = getColumn currentX . getRow currentY

done :: [[Char]] -> (Int, Int) -> Bool
done board (_, currentY) = currentY >= length board

traverseBoard :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int -> Int
traverseBoard board currentLocation stepToTake currentNumberOfTrees
    | done board currentLocation = currentNumberOfTrees
    | otherwise = traverseBoard board nextLocation stepToTake nextNumberOfTrees
    where nextLocation = step currentLocation stepToTake $ width board
          onTree = '#' == get currentLocation board
          nextNumberOfTrees = currentNumberOfTrees + if onTree then 1 else 0

readActual :: IO [[Char]]
readActual = fmap Text.unpack . Text.lines <$> Text.readFile "./data/data-day-03.txt"

traversePartOneActual :: IO Int 
traversePartOneActual = do
    board <- readActual
    return $ traverseBoard board (0, 0) (3, 1) 0

-- Part Two

partTwoExampleAnswer :: Int
partTwoExampleAnswer = 336

slopes :: [(Int, Int)]
slopes = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
    ]

traverseEach :: [[Char]] -> [(Int, Int)] -> [Int]
traverseEach board = fmap (\slope -> traverseBoard board (0, 0) slope 0)

traversePartTwoActual :: IO Int 
traversePartTwoActual = do
    board <- readActual
    return $ product $ traverseEach board slopes
