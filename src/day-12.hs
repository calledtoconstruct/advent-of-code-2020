import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either (fromRight)

type Location = (Int, Int)
type Direction = Int
type Waypoint = Location
type Ship = (Location, Direction, Waypoint)
type Distance = Int
type Angle = Int
type Program = [Text.Text]

data Instruction = Instruction {
    code :: Char,
    parameter :: Int
}

data Operation = Operation {
    operationCode :: Char,
    operation :: Ship -> Int -> Ship
}

startLocation :: Location
startLocation = (0, 0)

startDirection :: Direction
startDirection = 90

waypointStart :: Waypoint
waypointStart = (10, -1)

load :: Text.Text -> Instruction
load instruction = Instruction {
    code = Text.head instruction,
    parameter = fst $ fromRight (0, Text.pack "") $ Text.decimal $ Text.drop 1 instruction
}

step :: [Operation] -> Ship -> Program -> Ship
step operations ship program
    | null program = ship
    | otherwise = step operations result $ tail program
    where instruction = load $ head program
          function = operation $ head $ filter (\ op -> code instruction == operationCode op) operations
          result = function ship $ parameter instruction

loadProgram :: IO Program
loadProgram = do Text.lines <$> Text.readFile "./data/data-day-12.txt"

-- Part One

partOneOperations :: [Operation]
partOneOperations = [
    Operation { operationCode = 'N', operation = north },
    Operation { operationCode = 'S', operation = south },
    Operation { operationCode = 'E', operation = east },
    Operation { operationCode = 'W', operation = west },
    Operation { operationCode = 'R', operation = right },
    Operation { operationCode = 'L', operation = left },
    Operation { operationCode = 'F', operation = forward }
    ]

forward :: Ship -> Distance -> Ship
forward ship distance
    | angle == 0 = north ship distance
    | angle == 90 = east ship distance
    | angle == 180 = south ship distance
    | angle == 270 = west ship distance
    where (location, angle, _) = ship
          (x, y) = location

north :: Ship -> Distance -> Ship
north ((x, y), angle, waypoint) distance = ((x, y - distance), angle, waypoint)

south :: Ship -> Distance -> Ship
south ((x, y), angle, waypoint) distance = ((x, y + distance), angle, waypoint)

east :: Ship -> Distance -> Ship
east ((x, y), angle, waypoint) distance = ((x + distance, y), angle, waypoint)

west :: Ship -> Distance -> Ship
west ((x, y), angle, waypoint) distance = ((x - distance, y), angle, waypoint)

right :: Ship -> Angle -> Ship
right (location, angle', waypoint) angle = (location, (angle' + angle) `mod` 360, waypoint) 

left :: Ship -> Angle -> Ship
left (location, angle', waypoint) angle = (location, (360 + (angle' - angle)) `mod` 360, waypoint)

sampleProgram :: [Text.Text]
sampleProgram = [
    Text.pack "F10",
    Text.pack "N3",
    Text.pack "F7",
    Text.pack "R90",
    Text.pack "F11"
    ]

samplePartOne :: Int
samplePartOne = x + y
    where ((x, y), _, _) = step partOneOperations (startLocation, startDirection, waypointStart) sampleProgram

partOne :: IO Int
partOne = do
    program <- loadProgram
    let ((x, y), _, _) = step partOneOperations (startLocation, startDirection, waypointStart) program
    return $ abs x + abs y

-- Part Two

partTwoOperations :: [Operation]
partTwoOperations = [
    Operation { operationCode = 'N', operation = north' },
    Operation { operationCode = 'S', operation = south' },
    Operation { operationCode = 'E', operation = east' },
    Operation { operationCode = 'W', operation = west' },
    Operation { operationCode = 'R', operation = right' },
    Operation { operationCode = 'L', operation = left' },
    Operation { operationCode = 'F', operation = forward' }
    ]

forward' :: Ship -> Distance -> Ship
forward' ((x, y), angle, (x', y')) times = ((x - offsetX, y - offsetY), angle, (x' - offsetX, y' - offsetY))
    where offsetX = (x - x') * times
          offsetY = (y - y') * times

north' :: Ship -> Distance -> Ship
north' (location, angle, (x, y)) distance = (location, angle, (x, y - distance))

south' :: Ship -> Distance -> Ship
south' (location, angle, (x, y)) distance = (location, angle, (x, y + distance))

east' :: Ship -> Distance -> Ship
east' (location, angle, (x, y)) distance = (location, angle, (x + distance, y))

west' :: Ship -> Distance -> Ship
west' (location, angle, (x, y)) distance = (location, angle, (x - distance, y))

-- (2, 3) -> 90 -> (-3, 2) -> 90 -> (-2, -3) -> (3, -2)

right' :: Ship -> Angle -> Ship
right' (location, angle, waypoint) amount
    | amount == 0 = (location, angle, waypoint)
    | amount >= 90 = right' (location, angle, updated) (amount - 90)
    where (offsetX, offsetY) = location
          (waypointX, waypointY) = waypoint
          (x, y) = (waypointX - offsetX, waypointY - offsetY)
          (updatedX, updatedY) = (negate y, x)
          updated = (updatedX + offsetX, updatedY + offsetY)

-- (2, 3) -> 90 -> (3, -2) -> 90 -> (-2, -3) -> 90 -> (-3, 2)

left' :: Ship -> Angle -> Ship
left' (location, angle, waypoint) amount
    | amount == 0 = (location, angle, waypoint)
    | amount >= 90 = left' (location, angle, updated) (amount - 90)
    where (offsetX, offsetY) = location
          (waypointX, waypointY) = waypoint
          (x, y) = (waypointX - offsetX, waypointY - offsetY)
          (updatedX, updatedY) = (y, negate x)
          updated = (updatedX + offsetX, updatedY + offsetY)

samplePartTwo :: Int
samplePartTwo = x + y
    where ((x, y), _, _) = step partTwoOperations (startLocation, startDirection, waypointStart) sampleProgram

partTwo :: IO Int
partTwo = do
    program <- loadProgram
    let ((x, y), _, _) = step partTwoOperations (startLocation, startDirection, waypointStart) program
    return $ abs x + abs y