import qualified Data.Text as Text
import Data.Foldable

type Row = Text.Text
type Layer = [Row]
type Cube = [Layer]
type Location = (Int, Int, Int)

active :: Char
active = '#'

inactive :: Char
inactive = '.'

isActive :: Char -> Bool
isActive = (==) active

isInactive :: Char -> Bool
isInactive = (==) inactive

direction :: [(Int, Int, Int)]
direction = filter (/= (0, 0, 0)) $ concat $ (\ a -> concat $ (\ b -> (\ c -> (c, b, a)) <$> [-1, 0, 1]) <$> [-1, 0, 1]) <$> [-1, 0, 1]

inbounds :: Cube -> Location -> Bool
inbounds cube (x, y, z)
    | x < 0 || y < 0 = False
    | x >= width || y >= height || abs z >= depth = False
    | otherwise = True
    where width = Text.length $ head $ head cube
          height = length $ head cube
          depth = length cube

get :: Cube -> Location -> Char
get cube location@(x, y, z)
    | inbounds cube location = Text.index row x
    | otherwise = inactive
    where layer = cube !! abs z
          row = layer !! y

set :: Cube -> Location -> Char -> Cube
set cube location@(x, y, z) state = take z cube ++ (updatedLayer : drop (z + 1) cube)
    where layer = cube !! z
          row = layer !! y
          updatedRow = Text.concat [Text.take x row, Text.singleton state, Text.drop (x + 1) row]
          updatedLayer = take y layer ++ (updatedRow : drop (y + 1) layer)

applyDirection :: Location -> Location -> Location
applyDirection (x, y, z) (cx, cy, cz) = (x + cx, y + cy, z + cz)

shouldActivate :: Cube -> Location -> Bool
shouldActivate cube location@(x, y, z) = 3 == neighbors
    where neighbors = length $ filter isActive $ get cube . applyDirection location <$> direction

shouldDeactivate :: Cube -> Location -> Bool
shouldDeactivate cube location@(x, y, z) = 2 /= neighbors && 3 /= neighbors
    where neighbors = length $ filter isActive $ get cube . applyDirection location <$> direction

newRow :: Int -> Row
newRow size = Text.replicate size $ Text.singleton inactive

newLayer :: Int -> Layer
newLayer size = replicate size $ newRow size

expandRow :: Row -> Row
expandRow row = Text.concat [Text.singleton inactive, row, Text.singleton inactive]

expandLayer :: Layer -> Layer
expandLayer layer = concat [[newRow size], expandedLayer, [newRow size]]
    where expandedLayer = map expandRow layer
          size = Text.length $ head expandedLayer

expandCube :: Cube -> Cube
expandCube cube = expanded ++ [newLayer size]
    where expanded = map expandLayer cube
          size = Text.length $ head $ head expanded

newState :: Location -> Cube -> Char
newState location cube
    | isActive current && shouldDeactivate cube location = inactive
    | isInactive current && shouldActivate cube location = active
    | otherwise = current
    where current = get cube location

cycleCube :: Cube -> Cube
cycleCube cube = foldl' (\ c location -> set c location $ newState location expanded) expanded locations
    where expanded = expandCube cube
          width = Text.length $ head $ head expanded
          height = length $ head expanded
          depth = length expanded
          horizontal = [0..(width - 1)]
          vertical = [0..(height - 1)]
          deep = [0..depth - 1]
          locations = concat $ (\ a -> concat $ (\ b -> (\ c -> (c, b, a)) <$> horizontal) <$> vertical) <$> deep

loadPartOneSampleData :: Cube
loadPartOneSampleData = [[
    Text.pack ".#.",
    Text.pack "..#",
    Text.pack "###"
    ]]

partOne :: Cube -> Int
partOne cube = middleLayer + lowerLayers * 2
    where endCube = cycleCube $ cycleCube $ cycleCube $ cycleCube $ cycleCube $ cycleCube cube
          middleLayer = Text.length $ Text.filter (== active) $ Text.concat $ head endCube
          lowerLayers = Text.length $ Text.filter (== active) $ Text.concat $ concat $ tail endCube

testPartOne :: Bool
testPartOne = (==) 112 $ partOne loadPartOneSampleData

loadData :: Cube
loadData = [[
    Text.pack "..##.##.",
    Text.pack "#.#..###",
    Text.pack "##.#.#.#",
    Text.pack "#.#.##.#",
    Text.pack "###..#..",
    Text.pack ".#.#..##",
    Text.pack "#.##.###",
    Text.pack "#.#..##."
    ]]