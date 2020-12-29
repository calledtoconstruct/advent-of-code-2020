import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.List.Extra (sortOn)
import Data.Either (fromRight)

type BusNumber = Int
type Time = Int
type Notes = (Time, [BusNumber])

loadNotes :: IO Notes
loadNotes = do
    lines <- Text.lines <$> Text.readFile "./data/data-day-13.txt"
    let arrivalTime = fst . fromRight (0, Text.pack "") . Text.decimal $ head lines
    let buses = fst . fromRight (0, Text.pack "") . Text.decimal <$> Text.split (== ',') (last lines)
    return (arrivalTime, buses)

calculateTimeOfArrival :: [(BusNumber, Time)] -> [(BusNumber, Time)] -> [(BusNumber, Time)]
calculateTimeOfArrival inTransit arrivalTimes
    | null inTransit = arrivalTimes
    | otherwise = calculateTimeOfArrival (tail inTransit) $ (bus, arrivalTime): arrivalTimes
    where (bus, transitTime) = head inTransit
          arrivalTime = bus - transitTime

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
sampleNotes = (939, [7, 13, 0, 0, 59, 0, 31, 19])
