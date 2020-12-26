import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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
done board (_, currentY) = currentY == length board

traversePartOne :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int -> Int
traversePartOne board currentLocation stepToTake currentNumberOfTrees
    | done board currentLocation = nextNumberOfTrees
    | otherwise = traversePartOne board nextLocation stepToTake nextNumberOfTrees
    where nextLocation = step currentLocation stepToTake $ width board
          onTree = '#' == get currentLocation board
          nextNumberOfTrees = currentNumberOfTrees + if onTree then 1 else 0

readActual :: IO [[Char]]
readActual = fmap Text.unpack . Text.lines <$> Text.readFile "data-day-03.txt"

traversePartOneActual :: IO Int 
traversePartOneActual = do
    board <- readActual
    return $ traversePartOne board (0, 0) (3, 1) 0