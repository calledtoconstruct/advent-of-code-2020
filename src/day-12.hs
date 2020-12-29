import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either (fromRight)

type Location = (Int, Int)
type Direction = Int
type Ship = (Location, Direction)
type Distance = Int
type Angle = Int
type Program = [Text.Text]

startLocation :: Location
startLocation = (0, 0)

startDirection :: Direction
startDirection = 90

forward :: Ship -> Distance -> Ship
forward ship distance
    | angle == 0 = north ship distance
    | angle == 90 = east ship distance
    | angle == 180 = south ship distance
    | angle == 270 = west ship distance
    where (location, angle) = ship
          (x, y) = location

north :: Ship -> Distance -> Ship
north ((x, y), angle) distance = ((x, y - distance), angle)

south :: Ship -> Distance -> Ship
south ((x, y), angle) distance = ((x, y + distance), angle)

east :: Ship -> Distance -> Ship
east ((x, y), angle) distance = ((x + distance, y), angle)

west :: Ship -> Distance -> Ship
west ((x, y), angle) distance = ((x - distance, y), angle)

right :: Ship -> Angle -> Ship
right (location, angle') angle = (location, (angle' + angle) `mod` 360) 

left :: Ship -> Angle -> Ship
left (location, angle') angle = (location, (360 + (angle' - angle)) `mod` 360)

step :: Ship -> Program -> Ship
step ship program
    | null program = ship
    | code == 'N' = step (north ship parameter) $ tail program
    | code == 'S' = step (south ship parameter) $ tail program
    | code == 'E' = step (east ship parameter) $ tail program
    | code == 'W' = step (west ship parameter) $ tail program
    | code == 'R' = step (right ship parameter) $ tail program
    | code == 'L' = step (left ship parameter) $ tail program
    | code == 'F' = step (forward ship parameter) $ tail program
    where instruction = head program
          code = Text.head instruction
          parameter = fst $ fromRight (0, Text.pack "") $ Text.decimal $ Text.drop 1 instruction

loadProgram :: IO Program
loadProgram = do Text.lines <$> Text.readFile "./data/data-day-12.txt"

-- Part One

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
    where ((x, y), _) = step (startLocation, startDirection) sampleProgram

partOne :: IO Int
partOne = do
    program <- loadProgram
    let ((x, y), _) = step (startLocation, startDirection) program
    return $ abs x + abs y
