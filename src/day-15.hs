import qualified Data.IntMap as Map
import Data.Maybe (isJust)

type LastSpoken = Map.IntMap Int
type Iteration = Int
type Number = Int

initializeLastSpoken :: LastSpoken
initializeLastSpoken = Map.empty

speak :: LastSpoken -> (Number, Iteration) -> LastSpoken
speak lastSpoken (number, iteration) = Map.insert number iteration lastSpoken

calculateNext :: LastSpoken -> Number -> Iteration -> Number
calculateNext lastSeen number iteration
    | isJust $ lastSeen Map.!? number = iteration - (lastSeen Map.! number) 
    | otherwise = 0

calculate :: Iteration -> Iteration -> LastSpoken -> Number -> Number
calculate iteration until lastSpoken number
    | iteration == until = number
    | otherwise = calculate (iteration + 1) until (speak lastSpoken (number, iteration)) (calculateNext lastSpoken number iteration)

execute :: [Number] -> Number
execute seed = let
    number = last seed
    lastSpoken = foldl speak initializeLastSpoken $ zip seed [1..length seed]
    in calculate (length seed) 2020 lastSpoken number 

-- Part One

partOneData :: [Number]
partOneData = [12,20,0,6,1,17,7]

partOneSample :: [Number]
partOneSample = [0,3,6]

testPartOne :: [Bool]
testPartOne = [
    execute partOneSample == 436,
    execute [1,3,2] == 1,
    execute [2,1,3] == 10,
    execute [1,2,3] == 27,
    execute [2,3,1] == 78,
    execute [3,2,1] == 438,
    execute [3,1,2] == 1836
    ]

partOne :: Number
partOne = execute partOneData
