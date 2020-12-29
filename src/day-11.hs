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

shouldSit :: Map -> Location -> Bool 
shouldSit map location
    | isOccupied current = False
    | isFloor current = False
    | isAvailable current && occupied == 0 = True
    | otherwise = False
    where current = get map location
          (x, y) = location
          surroundedBy = get map <$> surrounding location
          occupied = length $ filter isOccupied surroundedBy

shouldStand :: Map -> Location -> Bool 
shouldStand map location
    | (not . isOccupied) current = False
    | isFloor current = False
    | (not . isAvailable) current && occupied > 3 = True
    | otherwise = False
    where current = get map location
          (x, y) = location
          surroundedBy = get map <$> surrounding location
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

walk :: Map -> Map -> Location -> Map
walk currentMap nextMap location
    | not isInbounds && currentMap == nextMap = nextMap
    | not isInbounds = walk nextMap nextMap (0, 0)
    | shouldSit currentMap location = walk currentMap (update nextMap location '#') $ next size location 
    | shouldStand currentMap location = walk currentMap (update nextMap location 'L') $ next size location 
    | otherwise = walk currentMap nextMap $ next size location
    where (mapData, size) = currentMap
          isInbounds = inbounds size location

loadMap :: IO Map
loadMap = do
    mapData <- Text.lines <$> Text.readFile "./data/data-day-11.txt"
    return (mapData, (Text.length $ head mapData, length mapData))

partOne :: IO Int
partOne = do
    map <- loadMap
    let (mapData, _) = walk map map (0, 0)
    let seated = Text.concat $ Text.filter (== '#') <$> mapData
    return $ Text.length seated
    