{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap as Map
import Data.Maybe (isJust)
import Data.List.Extra (foldl')

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
calculate !iteration !until !lastSpoken !number
    | iteration == until = number
    | otherwise = calculate (iteration + 1) until (speak lastSpoken (number, iteration)) (calculateNext lastSpoken number iteration)

execute :: [Number] -> Iteration -> Number
execute seed until = let
    number = last seed
    lastSpoken = foldl speak initializeLastSpoken $ zip seed [1..length seed]
    in calculate (length seed) until lastSpoken number 

numbers :: [Number]
numbers = [12,20,0,6,1,17,7]

-- Part One

partOneSample :: [Number]
partOneSample = [0,3,6]

testPartOne :: [Bool]
testPartOne = [
    execute partOneSample 2020 == 436,
    execute [1,3,2] 2020 == 1,
    execute [2,1,3] 2020 == 10,
    execute [1,2,3] 2020 == 27,
    execute [2,3,1] 2020 == 78,
    execute [3,2,1] 2020 == 438,
    execute [3,1,2] 2020 == 1836
    ]

partOne :: Number
partOne = execute numbers 2020

-- Part Two

testPartTwo :: [Bool]
testPartTwo = [
    execute [0,3,6] 30000000 == 175594,
    execute [1,3,2] 30000000 == 2578,
    execute [2,1,3] 30000000 == 3544142,
    execute [1,2,3] 30000000 == 261214,
    execute [2,3,1] 30000000 == 6895259,
    execute [3,2,1] 30000000 == 18,
    execute [3,1,2] 30000000 == 362
    ]

partTwo :: Number
partTwo = execute numbers 30000000




-- speak' :: LastSpoken -> (Number, Iteration) -> LastSpoken
-- speak' lastSpoken entry@(number, iteration)
--     | null after = entry: lastSpoken
--     | otherwise = entry: before ++ tail after
--     where (before, after) = break ((==) number . fst) lastSpoken

-- calculateNext' :: LastSpoken -> (Number, Iteration) -> Number
-- calculateNext' lastSpoken (number, iteration)
--     | isJust lastIteration = iteration - fromJust lastIteration
--     | otherwise = 0
--     where lastIteration = lookup number lastSpoken

-- calculate' :: (LastSpoken, Number) -> Iteration -> (LastSpoken, Number)
-- calculate' (lastSpoken, number) iteration = (speak' lastSpoken (number, iteration), calculateNext' lastSpoken (number, iteration))

-- execute' :: [Number] -> Iteration -> Number
-- execute' seed until = let
--     lastSpoken = foldl' speak' initializeLastSpoken $ zip seed [1..length seed]
--     in snd $ foldl' calculate' (lastSpoken, last seed) [length seed..until - 1]
