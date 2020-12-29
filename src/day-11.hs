import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type MapData = [Text.Text]
type Map = (MapData, (Int, Int))
type Location = (Int, Int)
type Size = (Int, Int)

sampleMap :: Map
sampleMap = ([Text.pack "L.LL.LL.LL",
    Text.pack "LLLLLLL.LL",
    Text.pack "L.L.L..L..",
    Text.pack "LLLL.LL.LL",
    Text.pack "L.LL.LL.LL",
    Text.pack "L.LLLLL.LL",
    Text.pack "..L.L.....",
    Text.pack "LLLLLLLLLL",
    Text.pack "L.LLLLLL.L",
    Text.pack "L.LLLLL.LL"], (10, 10))


isOccupied :: Char -> Bool 
isOccupied ch = ch == '#'

isFloor :: Char -> Bool 
isFloor ch = ch == '.'

isAvailable :: Char -> Bool 
isAvailable ch = ch == 'L'

inbounds :: (Int, Int) -> Location -> Bool 
inbounds (width, height) (x, y)
    | x < 0 = False
    | y < 0 = False
    | x >= width = False
    | y >= height = False
    | otherwise = True

get :: Map -> Location -> Char
get (map, size) location
    | inbounds size location = Text.index row x
    | otherwise = '.'
    where (width, height) = size
          (x, y) = location
          row = map !! y

northWest (x, y) = (x - 1, y - 1)
north (x, y) = (x, y - 1)
northEast (x, y) = (x + 1, y - 1)
west (x, y) = (x - 1, y)
east (x, y) = (x + 1, y)
southWest (x, y) = (x - 1, y + 1)
south (x, y) = (x, y + 1)
southEast (x, y) = (x + 1, y + 1)

surrounding :: Location -> [Location]
surrounding location = [
    northWest location, north location, northEast location,
    west location, east location,
    southWest location, south location, southEast location
    ]

shouldSit :: [Char] -> Map -> Location -> Bool 
shouldSit surroundedBy map location
    | isOccupied current = False
    | isFloor current = False
    | isAvailable current && occupied == 0 = True
    | otherwise = False
    where current = get map location
          (x, y) = location
          occupied = length $ filter isOccupied surroundedBy

shouldStand :: [Char] -> Map -> Location -> Int -> Bool 
shouldStand surroundedBy map location tolerance
    | (not . isOccupied) current = False
    | isFloor current = False
    | (not . isAvailable) current && occupied >= tolerance = True
    | otherwise = False
    where current = get map location
          (x, y) = location
          occupied = length $ filter isOccupied surroundedBy

next :: (Int, Int) -> Location -> Location
next (width, height) (x, y)
    | nextX < width = (nextX, y)
    | otherwise = (0, nextY)
    where nextX = x + 1
          nextY = y + 1

update :: Map -> Location -> Char -> Map
update (mapData, size) (x, y) to = (rowsBefore ++ [updated] ++ rowsAfter, size)
    where row = mapData !! y
          seatsBefore = Text.take x row
          seatsAfter = Text.drop (x + 1) row
          parts = [seatsBefore, Text.pack [to], seatsAfter]
          updated = Text.concat parts
          rowsBefore = take y mapData
          rowsAfter = drop (y + 1) mapData

surroundingSeats :: Map -> Location -> [Char]
surroundingSeats map location = get map <$> surrounding location

walk :: (Map -> Location -> [Char]) -> Map -> Map -> Location -> Int -> Map
walk surrounding currentMap nextMap location tolerance
    | not isInbounds && currentMap == nextMap = nextMap
    | not isInbounds = walk surrounding nextMap nextMap (0, 0) tolerance
    | shouldSit surroundedBy currentMap location = walk surrounding currentMap (update nextMap location '#') nextLocation tolerance
    | shouldStand surroundedBy currentMap location tolerance = walk surrounding currentMap (update nextMap location 'L') nextLocation tolerance
    | otherwise = walk surrounding currentMap nextMap nextLocation tolerance
    where (mapData, size) = currentMap
          isInbounds = inbounds size location
          surroundedBy = surrounding currentMap location
          nextLocation = next size location

loadMap :: IO Map
loadMap = do
    mapData <- Text.lines <$> Text.readFile "./data/data-day-11.txt"
    return (mapData, (Text.length $ head mapData, length mapData))

partOne :: IO Int
partOne = do
    map <- loadMap
    let (mapData, _) = walk surroundingSeats map map (0, 0) 4
    let seated = Text.concat $ Text.filter (== '#') <$> mapData
    return $ Text.length seated

-- Part Two

look :: (Location -> Location) -> Map -> Location -> Char
look direction map location
    | not $ inbounds size location = '.'
    | isOccupied current = current
    | isAvailable current = current
    | otherwise = look direction map next
    where (_, size) = map
          next = direction location
          current = get map next

lineOfSight :: Map -> Location -> [Char]
lineOfSight map location = found
    where nw = look northWest map location
          n = look north map location
          ne = look northEast map location
          w = look west map location
          e = look east map location
          sw = look southWest map location
          s = look south map location
          se = look southEast map location
          found = [nw, n, ne, w, e, sw, s, se]

partTwo :: IO Int
partTwo = do
    map <- loadMap
    let (mapData, _) = walk lineOfSight map map (0, 0) 5
    let seated = Text.concat $ Text.filter (== '#') <$> mapData
    return $ Text.length seated
