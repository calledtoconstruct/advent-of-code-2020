module DayThirteen where
    
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List.Extra (sortOn)
import Data.Either (fromRight)
import Control.Parallel.Strategies (rpar, using)

type BusNumber = Int
type Time = Int
type Notes = (Time, [BusNumber])
type BusAndOffset = (BusNumber, Time)
data Bus = Bus {
    busNumber :: BusNumber,
    offset :: Time
}

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

partTwo :: IO Time
partTwo = do start . snd <$> loadNotes

findBusAlignment :: (Time -> Time, Time) -> Bus -> Time
findBusAlignment (increment, currentTime) bus
    | isAligned = currentTime
    | otherwise = findBusAlignment (increment, increment currentTime) bus
    where isAligned = (== 0) $ mod (currentTime - offset bus) $ busNumber bus

findAlignment :: Time -> Time -> [Bus] -> Time
findAlignment interval currentTime buses
    | null buses = currentTime
    | otherwise = findAlignment (interval * busNumber (head buses)) next (tail buses)
    where next = curry findBusAlignment (interval +) currentTime $ head buses

loadBus :: BusAndOffset -> Bus
loadBus (bus, time) = Bus { busNumber = bus, offset = time }

start :: [BusNumber] -> Time
start buses = (-) alignedTime $ length buses - 1
    where queue = filter ((/= 0) . busNumber) $ loadBus <$> zip (reverse buses) [0..]
          interval = busNumber $ head queue
          startingAt = interval
          alignedTime = findAlignment interval startingAt $ tail queue

runTests :: [Bool]
runTests = [
    start [17,0,13,19] == 3417,
    start [67,7,59,61] == 754018,
    start [67,0,7,59,61] == 779210,
    start [67,7,0,59,61] == 1261476,
    start [1789,37,47,1889] == 1202161486,
    start [7, 13, 0, 0, 59, 0, 31, 19] == 1068781
    ]

showTestOutput :: [Time]
showTestOutput = [
    start [17,0,13,19],
    start [67,7,59,61],
    start [67,0,7,59,61],
    start [67,7,0,59,61],
    start [1789,37,47,1889],
    start [7, 13, 0, 0, 59, 0, 31, 19]
    ]
