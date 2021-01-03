import qualified Data.Text as Text
import Data.Foldable

type Row = Text.Text
type Layer = [Row]
type Cube = [Layer]
type HyperCube = [Cube]
type Location1D = Int
type Location2D = (Int, Int)
type Location3D = (Int, Int, Int)
type Location4D = (Int, Int, Int, Int)

active :: Char
active = '#'

inactive :: Char
inactive = '.'

isActive :: Char -> Bool
isActive = (==) active

isInactive :: Char -> Bool
isInactive = (==) inactive

newRow :: Int -> Row
newRow size = Text.replicate size $ Text.singleton inactive

newLayer :: Int -> Layer
newLayer size = replicate size $ newRow size

newCube :: Int -> Cube
newCube size = replicate size $ newLayer size

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

expandHyperCube :: HyperCube -> HyperCube
expandHyperCube hyperCube = expanded ++ [newCube size]
    where expanded = map expandCube hyperCube
          size = Text.length $ head $ head $ head expanded

direction1D :: [Location1D]
direction1D = [-1, 0, 1]

direction2D :: [Location2D]
direction2D = concat $ (\ a -> (\ b -> (b, a)) <$> direction1D) <$> direction1D

direction3D :: [Location3D]
direction3D = concat $ (\ (a, b) -> (\ c -> (c, b, a)) <$> direction1D) <$> direction2D

direction4D :: [Location4D]
direction4D = concat $ (\ (a, b, c) -> (\ d -> (d, c, b, a)) <$> direction1D) <$> direction3D

inRow :: Row -> Location1D -> Bool
inRow row x
    | x < 0 = False
    | x >= width = False
    | otherwise = True
    where width = Text.length row

inLayer :: Layer -> Location2D -> Bool
inLayer layer (x, y)
    | y < 0 || y >= height = False
    | otherwise = inRow (head layer) x
    where height = length layer

inCube :: Cube -> Location3D -> Bool
inCube cube (x, y, z)
    | abs z >= depth = False
    | otherwise = inLayer (head cube) (x, y)
    where depth = length cube

inHyperCube :: HyperCube -> Location4D -> Bool
inHyperCube hyperCube (x, y, z, t)
    | abs t >= time = False
    | otherwise = inCube (head hyperCube) (x, y, z)
    where time = length hyperCube

getFromRow :: Row -> Location1D -> Char
getFromRow = Text.index

getFromLayer :: Layer -> Location2D -> Char
getFromLayer layer location@(x, y)
    | inLayer layer location = getFromRow (layer !! y) x
    | otherwise = inactive

getFromCube :: Cube -> Location3D -> Char
getFromCube cube location@(x, y, z)
    | inCube cube location = getFromLayer (cube !! abs z) (x, y)
    | otherwise = inactive

getFromHyperCube :: HyperCube -> Location4D -> Char
getFromHyperCube hyperCube location@(x, y, z, t)
    | inHyperCube hyperCube location = getFromCube (hyperCube !! abs t) (x, y, z)
    | otherwise = inactive

setInRow :: Row -> Location1D -> Char -> Row
setInRow row location state = Text.concat [Text.take location row, Text.singleton state, Text.drop (location + 1) row]

setInLayer :: Layer -> Location2D -> Char -> Layer
setInLayer layer location@(x, y) state = take y layer ++ (setInRow (layer !! y) x state : drop (y + 1) layer)

setInCube :: Cube -> Location3D -> Char -> Cube
setInCube cube location@(x, y, z) state = take z cube ++ (setInLayer (cube !! z) (x, y) state : drop (z + 1) cube)

setInHyperCube :: HyperCube -> Location4D -> Char -> HyperCube
setInHyperCube hyperCube location@(x, y, z, t) state = take t hyperCube ++ (setInCube (hyperCube !! t) (x, y, z) state : drop (t + 1) hyperCube)

shouldActivate :: Int -> Bool
shouldActivate neighbors = 3 == neighbors

shouldDeactivate :: Int -> Bool
shouldDeactivate neighbors = 2 /= neighbors && 3 /= neighbors

makeCubeFromLayer :: Layer -> Cube
makeCubeFromLayer layer = [layer]

makeHyperCubeFromCube :: Cube -> HyperCube
makeHyperCubeFromCube cube = [cube]

sampleData :: Cube
sampleData = makeCubeFromLayer [
    Text.pack ".#.",
    Text.pack "..#",
    Text.pack "###"
    ]

loadData :: Cube
loadData = makeCubeFromLayer [
    Text.pack "..##.##.",
    Text.pack "#.#..###",
    Text.pack "##.#.#.#",
    Text.pack "#.#.##.#",
    Text.pack "###..#..",
    Text.pack ".#.#..##",
    Text.pack "#.##.###",
    Text.pack "#.#..##."
    ]

-- Part One

applyDirection3D :: Location3D -> Location3D -> Location3D
applyDirection3D (x, y, z) (cx, cy, cz) = (x + cx, y + cy, z + cz)

neighbors3D :: Cube -> Location3D -> Int
neighbors3D cube location = length $ filter isActive $ getFromCube cube . applyDirection3D location <$> filter (/= (0, 0, 0)) direction3D

newState3D :: Location3D -> Cube -> Char
newState3D location cube
    | isActive current && shouldDeactivate neighbors = inactive
    | isInactive current && shouldActivate neighbors = active
    | otherwise = current
    where current = getFromCube cube location
          neighbors = neighbors3D cube location

cycleCube :: Cube -> Cube
cycleCube cube = foldl' (\ c location -> setInCube c location $ newState3D location expanded) expanded locations
    where expanded = expandCube cube
          width = Text.length $ head $ head expanded
          height = length $ head expanded
          depth = length expanded
          horizontal = [0..(width - 1)]
          vertical = [0..(height - 1)]
          deep = [0..depth - 1]
          locations = concat $ (\ a -> concat $ (\ b -> (\ c -> (c, b, a)) <$> horizontal) <$> vertical) <$> deep

countEnabledInCube :: Cube -> Int
countEnabledInCube cube = middleLayer + lowerLayers * 2
    where middleLayer = Text.length $ Text.filter (== active) $ Text.concat $ head cube
          lowerLayers = Text.length $ Text.filter (== active) $ Text.concat $ concat $ tail cube

partOne :: Cube -> Int
partOne = countEnabledInCube . cycleCube . cycleCube . cycleCube . cycleCube . cycleCube . cycleCube

testPartOne :: Bool
testPartOne = (==) 112 $ partOne sampleData

-- Part Two

applyDirection4D :: Location4D -> Location4D -> Location4D
applyDirection4D (x, y, z, t) (cx, cy, cz, ct) = (x + cx, y + cy, z + cz, t + ct)

neighbors4D :: HyperCube -> Location4D -> Int
neighbors4D hyperCube location = length $ filter isActive $ getFromHyperCube hyperCube . applyDirection4D location <$> filter (/= (0, 0, 0, 0)) direction4D

newState4D :: Location4D -> HyperCube -> Char
newState4D location hyperCube
    | isActive current && shouldDeactivate neighbors = inactive
    | isInactive current && shouldActivate neighbors = active
    | otherwise = current
    where current = getFromHyperCube hyperCube location
          neighbors = neighbors4D hyperCube location

cycleHyperCube :: HyperCube -> HyperCube
cycleHyperCube hyperCube = foldl' (\ c location -> setInHyperCube c location $ newState4D location expanded) expanded locations
    where expanded = expandHyperCube hyperCube
          width = Text.length $ head $ head $ head expanded
          height = length $ head $ head expanded
          depth = length $ head expanded
          duration = length expanded
          horizontal = [0..(width - 1)]
          vertical = [0..(height - 1)]
          deep = [0..depth - 1]
          age = [0..duration - 1]
          locations = concat $ (\ a -> concat $ (\ b -> concat $ (\ c -> (\ d -> (d, c, b, a)) <$> horizontal) <$> vertical) <$> deep) <$> age

partTwo :: HyperCube -> Int
partTwo hyperCube = middleCube + lowerCubes * 2
    where endHyperCube = cycleHyperCube $ cycleHyperCube $ cycleHyperCube $ cycleHyperCube $ cycleHyperCube $ cycleHyperCube hyperCube
          middleCube = countEnabledInCube $ head endHyperCube
          lowerCubes = sum $ countEnabledInCube <$> tail endHyperCube

testPartTwo :: Bool
testPartTwo = (==) 848 $ partTwo $ makeHyperCubeFromCube sampleData
