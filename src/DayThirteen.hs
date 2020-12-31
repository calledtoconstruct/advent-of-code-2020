module DayThirteen where
    
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List.Extra (sortOn)
import Data.Either (fromRight)
import Data.Numbers.Primes (primes,  primeFactors)
import Data.Bifunctor
import GHC.Conc (par)
import Control.Parallel.Strategies (rpar, using)

type BusNumber = Int
type Time = Int
type Notes = (Time, [BusNumber])
type BusAndOffset = (BusNumber, Time)

loadNotes :: IO Notes
loadNotes = do
    lines <- Text.lines <$> Text.readFile "./data/data-day-13.txt"
    let arrivalTime = fst . fromRight (0, Text.pack "") . Text.decimal $ head lines
    let buses = fst . fromRight (0, Text.pack "") . Text.decimal <$> Text.split (== ',') (last lines)
    return (arrivalTime, buses)

calculateTimeOfArrival :: [BusAndOffset] -> [BusAndOffset] -> [BusAndOffset]
calculateTimeOfArrival inTransit arrivalTimes
    | null inTransit = arrivalTimes
    | otherwise = calculateTimeOfArrival buses $ (bus, bus - transitTime): arrivalTimes
    where ((bus, transitTime): buses) = inTransit

findFirstAvailableBus :: Notes -> Int
findFirstAvailableBus (arrival, busSchedule) = earliestBus * earliestTime
    where activeBuses = filter (/= 0) busSchedule
          transitTime = mod arrival <$> activeBuses
          inTransit = zip activeBuses transitTime
          arrivalTimes = calculateTimeOfArrival inTransit []
          (earliestBus, earliestTime) = head $ sortOn snd arrivalTimes

-- Part One

partOne :: IO Int
partOne = findFirstAvailableBus <$> loadNotes

sampleNotes :: Notes
-- sampleNotes = (939, [7, 13, 0, 0, 59, 0, 31, 19])
-- answer for sample data: 1068781
sampleNotes = (939, [17, 0, 13, 19])
-- answer for sample data: 3417

-- Part Two

findTimeOfAlignment :: Notes -> Time -> Time -> Time
findTimeOfAlignment notes@(_, buses) time skip
    | isAlignmentIncorrect time activeBusAndOffset = findTimeOfAlignment notes (time + skip) skip
    | otherwise = time - maxOffset
    where busAndOffset = zip (reverse buses) [0..]
          activeBusAndOffset = filter ((/= 0) . fst) busAndOffset
          maxOffset = maximum $ snd <$> activeBusAndOffset

partTwoBruteForce :: IO Time
partTwoBruteForce = do
    notes <- loadNotes
    return $ findTimeOfAlignment notes (last $ snd notes) (last $ snd notes)

routeAlignment :: Int -> BusAndOffset -> Bool
routeAlignment time (bus, offset) = (time - offset) `mod` bus == 0

isAlignmentIncorrect :: Time -> [BusAndOffset] -> Bool
isAlignmentIncorrect time = not . all (routeAlignment time)

findAlignment :: Time -> Time -> [BusAndOffset] -> [BusAndOffset] -> Time
findAlignment _ time _ [] = time
findAlignment timeInterval time pastBuses futureBuses
    | isAlignmentIncorrect time currentBuses = findAlignment timeInterval (time + timeInterval) pastBuses futureBuses
    | otherwise = findAlignment timeInterval time currentBuses nextBuses
    where current: nextBuses = futureBuses
          currentBuses = current: pastBuses

findAlignment' :: (Time -> Time) -> Time -> [BusAndOffset] -> [BusAndOffset] -> Time
findAlignment' nextTime time pastBuses futureBuses
    | null futureBuses = time
    | isAlignmentIncorrect time currentBuses = findAlignment' nextTime (nextTime time) pastBuses futureBuses
    | otherwise = findAlignment' nextTime time currentBuses nextBuses
    where current: nextBuses = futureBuses
          currentBuses = current: pastBuses

calculateAlignment :: [BusNumber] -> Time
calculateAlignment buses = flip (-) maxOffset $ findAlignment' (timeInterval +) startAt [] activeBusAndOffset
    where activeBusAndOffset = filter ((/= 0) . fst) $ zip (reverse buses) [0..]
          busNumbers = fst <$> activeBusAndOffset
          timeInterval = max (head busNumbers) $ maximum busNumbers `div` head busNumbers
          startAt = product $ init busNumbers
          timeOffsets = snd <$> activeBusAndOffset
          maxOffset = maximum timeOffsets

runTests :: Bool 
runTests = 
    calculateAlignment [17,0,13,19] == 3417 &&
    calculateAlignment [67,7,59,61] == 754018 &&
    calculateAlignment [67,0,7,59,61] == 779210 &&
    calculateAlignment [67,7,0,59,61] == 1261476 &&
    calculateAlignment [1789,37,47,1889] == 1202161486

partTwo :: IO Time
partTwo = do calculateAlignment . snd <$> loadNotes

_nextBuses :: ([BusAndOffset], [BusAndOffset]) -> Bool -> ([BusAndOffset], [BusAndOffset])
_nextBuses buses True = buses
_nextBuses (pastBuses, futureBuses) False = (currentBuses, nextBuses)
    where current: nextBuses = futureBuses
          currentBuses = current: pastBuses

_nextTimes :: (Time -> Time, Time) -> Bool -> (Time -> Time, Time)
_nextTimes times False = times
_nextTimes (nextTime, time) True = (nextTime, nextTime time)

_next :: (Time -> Time, Time) -> ([BusAndOffset], [BusAndOffset]) -> ((Time -> Time, Time), ([BusAndOffset], [BusAndOffset]))
_next times@(_, time) buses@(past, future) = (nextTimes, nextBuses)
    where isIncorrect = any (\ (bus, offset) -> (/= offset) $ mod time bus) (head future: past)
          nextTimes = _nextTimes times isIncorrect
          nextBuses = _nextBuses buses isIncorrect

_findAlignment' :: (Time -> Time, Time) -> ([BusAndOffset], [BusAndOffset]) -> Time
_findAlignment' (_, time) (_, []) = time
_findAlignment' times buses = (uncurry _findAlignment' . _next times) buses

_calculateAlignment :: [BusNumber] -> Time
_calculateAlignment buses = _findAlignment' times busQueue - length buses + 1
    where activeBuses = filter (/= 0) buses
          timeInterval = max (last activeBuses) $ maximum buses `div` last buses
          startAt = product $ tail activeBuses
          times = ((timeInterval +), startAt)
          busQueue = ([], filter ((/= 0) . fst) $ zip (reverse buses) [0..])

fa :: (Time -> Time, Time) -> BusAndOffset -> Time
fa times@(next, current) (bus, offset)
    | isIncorrect = fa (next, next current) (bus, offset)
    | otherwise = current
    where isIncorrect = (/= mod offset bus) $ mod current bus

ca :: Time -> Time -> [BusAndOffset] -> (Time -> Time -> Time) -> Time
ca interval current buses op
    | next == current = current
    | otherwise = ca interval next buses op `using` rpar
    where next = maximum (curry fa (op interval) current <$> buses) `using` rpar

start :: [BusNumber] -> Time
start buses = ca seed startAt (tail queue) (+) - (length buses - 1)
    where queue = filter ((/= 0) . fst) $ zip (reverse buses) [0..]
          seed = fst $ head queue
          startAt = seed



-- ca 19 (513+19) [(17,3)]
-- [190, 513, 836]
-- [[2,5,19],[3,3,3,19],[2,2,11,19]]
-- [10, 27, 44]
-- 17

-- For 19 and (19, 0) the equation is:
-- Given:
-- a = 19
-- b = 19
-- c = 0
-- d = 9
-- x = 9
-- q = (x*b^2) + (d*b) + c

-- For 19 and (13, 1) the equation is:
-- Given:
-- a = 19
-- b = 13
-- c = 1
-- d = 16
-- x = 19
-- q = (x*b^2) + (d*b) + c

-- For 19 and (17, 3) the equation is:
-- Given:
-- a = 19
-- b = 17
-- c = 3
-- d = 14
-- x = 11
-- q = (x*b^2) + (d*b) + c


-- ca 19 (2185+19) [(13,1)]
-- [209, 703, 950, 1197, 1444, 1691, 1938, 2185, 2432]
-- [[11,19],[19,37],[2,5,5,19],[3,3,7,19],[2,2,19,19],[19,89],[2,3,17,19],[5,19,23],[2,2,2,2,2,2,2,19]]
-- [11, 37, 50, 63, 76, 89, 102]
-- 13



-- ca :: [BusNumber] -> [Time]
-- ca buses = fmap (\busAndOffset -> _findAlignment' ((+(snd busAndOffset)), (fst busAndOffset)) ([], [busAndOffset])) busSchedule
--     where busSchedule = filter ((/= 0) . fst) $ zip (reverse buses) [0..]

-- [    17  ], x,[    13  ],[     19      ]
-- [ 3, 67  ],  ,[   263  ],[2, 2, 3, 3, 5]
-- [   201  ],  ,[   263  ],[     180     ]
-- p13 323
--    3417   ,  ,   3419   ,     3420
-- p  4199

-- div (3400 - 2) 13 = 261  (+2) = 261
-- div (3434 - 2) 13 = 264  (+2) = 264
-- mod (3400 - 2) 13 = 5    (+2) = 9
-- div (3400 - 3) 19 = 178  (+3) = 179
-- div (3434 - 3) 19 = 180  (+3) = 180
-- mod (3400 - 3) 19 = 15   (+3) = 2

--          67  ,       7  ,            59  ,      61
-- [  2,17,331 ],[ 107717 ],[ 2,2,3,3,5,71 ],[ 47,263 ]
-- [     11254 ],[ 107717 ],[        12780 ],[  12361 ]
-- p7   241133
--      754018  ,  754019  ,        754020  ,  754021
-- p   1687931

-- [[2,17,67,331],[7,107717],[2,2,3,3,5,59,71],[47,61,263]]

-- a = 61
-- b = 61
-- c = 0
-- d = 39
-- x = 202
-- q = (x*b^2) + (d*b) + c

-- a = 61
-- b = 59
-- c = 1
-- d = 36
-- x = 216
-- q = (x*b^2) + (d*b) + c

-- a = 61
-- b = 7
-- c = 2
-- d = 36
-- x = 15383
-- q = (x*b^2) + (d*b) + c

-- a = 61
-- b = 67
-- c = 3
-- d = 65
-- x = 167
-- q = (x*b^2) + (d*b) + c

-- a = 61
-- b = 67
-- c = 3
-- x = 11254
-- q = (x*b) + c


rt :: [Bool]
rt = [
    start [17,0,13,19] == 3417,
    start [67,7,59,61] == 754018,
    start [67,0,7,59,61] == 779210,
    start [67,7,0,59,61] == 1261476,
    start [1789,37,47,1889] == 1202161486,
    start [7, 13, 0, 0, 59, 0, 31, 19] == 1068781
    ]

rt' :: [Time]
rt' = [
    start [17,0,13,19],
    start [67,7,59,61],
    start [67,0,7,59,61],
    start [67,7,0,59,61],
    start [1789,37,47,1889]
    ]


_runTests :: Bool 
_runTests = 
    _calculateAlignment [17,0,13,19] == 3417 &&
    _calculateAlignment [67,7,59,61] == 754018 &&
    _calculateAlignment [67,0,7,59,61] == 779210 &&
    _calculateAlignment [67,7,0,59,61] == 1261476 &&
    _calculateAlignment [1789,37,47,1889] == 1202161486

-- n = 3420
-- d = 17
-- r = n - d * floor (n / d)

mergePrimeFactors :: [Int] -> [Int] -> [Int] -> [Int]
mergePrimeFactors output input input'
    | null input && null input' = reverse output
    | null input = mergePrimeFactors (head input': output) input tInput'
    | null input' = mergePrimeFactors (head input: output) tInput input'
    | head input < head input' = mergePrimeFactors (head input: output) tInput input'
    | head input > head input' = mergePrimeFactors (head input': output) input tInput'
    | head input == head input' = mergePrimeFactors (head input: output) tInput tInput'
    where tInput = tail input
          tInput' = tail input'

-- a = flip (-) 1 $ findAlignment [(13, 1)] [] 12 0
-- a' = flip (-) 3 $ findAlignment [(17, 3)] [] 14 0
-- x = sum $ foldl1 (mergePrimeFactors []) $ primeFactors <$> [a, a']
-- y = product [17, 13, 19]
-- z = y - (x * 19)


-- b = flip (-) 1 $ findAlignment [(59, 1)] [] 58 0
-- b' = flip (-) 2 $ findAlignment [(7, 2)] [] 5 0
-- b'' = flip (-) 3 $ findAlignment [(67, 3)] [] 64 0
-- b''' = flip (-) 0 $ findAlignment [(61, 0)] [] 61 61
-- c = foldl1 (mergePrimeFactors []) $ primeFactors <$> [b, b', b'', b''']
-- d = product [67,7,59,61]
-- e = d - ((sum $ c) * 61)


calculateCommonPrimeFactors :: [BusNumber] -> Int
calculateCommonPrimeFactors buses = product commonPrimeFactors
    where commonPrimeFactors = foldl1 (mergePrimeFactors []) $ primeFactors <$> filter (/= 0) buses

partTwoSample :: Notes -> Time
partTwoSample (_, buses) = calculateCommonPrimeFactors $ uncurry (+) <$> activeBusAndOffset
    where busAndOffset = zip (reverse buses) [0..]
          activeBusAndOffset = filter ((/= 0) . fst) busAndOffset
          maxOffset = maximum $ snd <$> activeBusAndOffset

-- 13n + 2 = 17m + 3

-- (3, 17), (2, 0), (1, 13), (0, 19) -> 3417.
-- (3, 17), (1, 13), (0, 19) -> 3417 + 3 = 3420
-- [(3420 - 3) `mod` 17, (3420 - 1) `mod` 13, 3420 `mod` 19]

-- [(x `mod` bus - offset == 0)]