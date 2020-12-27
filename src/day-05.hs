import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List (sort)
import Data.Maybe (fromJust)

-- The first 7 characters will either be F or B; these specify exactly one of 
-- the 128 rows on the plane (numbered 0 through 127). Each letter tells you
-- which half of a region the given seat is in. Start with the whole list of
-- rows; the first letter indicates whether the seat is in the front (0 through 63)
-- or the back (64 through 127). The next letter indicates which half of that region
-- the seat is in, and so on until you're left with exactly one row.

-- For example, consider just the first seven characters of FBFBBFFRLR:

-- Start by considering the whole range, rows 0 through 127.
-- F means to take the lower half, keeping rows 0 through 63.
-- B means to take the upper half, keeping rows 32 through 63.
-- F means to take the lower half, keeping rows 32 through 47.
-- B means to take the upper half, keeping rows 40 through 47.
-- B keeps rows 44 through 47.
-- F keeps rows 44 through 45.
-- The final F keeps the lower of the two, row 44.
-- The last three characters will be either L or R; these specify exactly one of
-- the 8 columns of seats on the plane (numbered 0 through 7). The same process
-- as above proceeds again, this time with only three steps. L means to keep the
-- lower half, while R means to keep the upper half.

-- For example, consider just the last 3 characters of FBFBBFFRLR:

-- Start by considering the whole range, columns 0 through 7.
-- R means to take the upper half, keeping columns 4 through 7.
-- L means to take the lower half, keeping columns 4 through 5.
-- The final R keeps the upper of the two, column 5.
-- So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

-- Every seat also has a unique seat ID: multiply the row by 8, then add the column.
-- In this example, the seat has ID 44 * 8 + 5 = 357.

-- Here are some other boarding passes:

-- BFFFBBFRRR: row 70, column 7, seat ID 567.
-- FFFBBBFRRR: row 14, column 7, seat ID 119.
-- BBFFBBFRLL: row 102, column 4, seat ID 820.
-- As a sanity check, look through your list of boarding passes. What is the highest
-- seat ID on a boarding pass?

calculateOffset :: Int -> Int 
calculateOffset current = div (current + 1) 2

type Range = (Int, Int)
type MaybeRange = (Maybe Int, Maybe Int)
type MaybeIdentity = Maybe Int

applyRow :: Range -> Char -> Range
applyRow range direction
    | direction == 'F' = (lower, (-) upper $ calculateOffset $ upper - lower)
    | direction == 'B' = ((+) lower $  calculateOffset $ upper - lower, upper)
    where (lower, upper) = range

applyColumn :: Range -> Char -> Range
applyColumn range direction
    | direction == 'L' = (lower, (-) upper $ calculateOffset $ upper - lower)
    | direction == 'R' = ((+) lower $  calculateOffset $ upper - lower, upper)
    where (lower, upper) = range

collapse :: Range -> Maybe Int
collapse range
    | lower == upper = Just lower
    | otherwise = Nothing
    where (lower, upper) = range

calculateIdentity :: MaybeRange -> MaybeIdentity
calculateIdentity maybeRange = case maybeRange of
    (Just row, Just column) -> Just $ (+) column $ 8 * row
    _ -> Nothing

collapseRange :: (Range, Range) -> MaybeRange
collapseRange (row, column) = (collapse row, collapse column)

apply :: (Text.Text, Text.Text) -> (Range, Range)
apply (rowCode, columnCode) = (Text.foldl applyRow (0, 127) rowCode, Text.foldl applyColumn (0, 7) columnCode)

splitBoardingPass :: Text.Text -> (Text.Text, Text.Text)
splitBoardingPass = Text.splitAt 7

maxIdentity :: Int -> [MaybeIdentity] -> MaybeIdentity
maxIdentity max [] = Just max
maxIdentity max identities = case x of
    Just identity
        | identity > max -> maxIdentity identity xs
        | identity <= max -> maxIdentity max xs
        | otherwise -> Nothing
    Nothing -> Nothing
    where x: xs = identities

partOne :: IO MaybeIdentity
partOne = do
    boardingPasses <- readBoardingPasses
    return $ maxIdentity 0 $ calculateIdentity . collapseRange . apply . splitBoardingPass <$> boardingPasses

testCalculateIdentity :: MaybeIdentity
testCalculateIdentity = calculateIdentity $ collapseRange (testApplyRow, testApplyColumn)

testApplyRow :: Range
testApplyRow = Text.foldl applyRow (0, 127) $ fst testSplitBoardingPass

testApplyColumn :: Range
testApplyColumn = Text.foldl applyColumn (0, 7) $ snd testSplitBoardingPass

testSplitBoardingPass :: (Text.Text, Text.Text)
testSplitBoardingPass = splitBoardingPass $ Text.pack "FBFBBFFRLR"

readBoardingPasses :: IO [Text.Text]
readBoardingPasses = Text.lines <$> Text.readFile "./data/data-day-05.txt"

-- Part Two

orderIdentities :: [MaybeIdentity] -> [MaybeIdentity]
orderIdentities = sort

--  [3, 4, 5, 6, 8, 9]
--  [3, 4, 5] = 2           [6, 8, 9] = 3
--  [6, 8, 9]
--  [6, 8] = 2              [ 8, 9] = 1
--  (6, 8) => answer 7

missing :: [MaybeIdentity] -> MaybeIdentity
missing [x, y] = Just $ div (fromJust x + fromJust y) 2
missing identities
    | lowerSpread > upperSpread = missing lower
    | lowerSpread < upperSpread = missing upper
    | otherwise = missing [last lower, head upper]
    where lower = take lowerEnd identities
          upper = drop upperStart identities
          lowerEnd = flip div 2 $ identitiesLength + 1
          upperStart = div identitiesLength 2
          identitiesLength = length identities
          lowerMinimum = head lower
          lowerMaximum = last lower
          upperMinimum = head upper
          upperMaximum = last upper
          lowerSpread = fromJust lowerMaximum - fromJust lowerMinimum
          upperSpread = fromJust upperMaximum - fromJust upperMinimum

partTwo :: IO MaybeIdentity
partTwo = do
    boardingPasses <- readBoardingPasses
    let ordered = orderIdentities $ calculateIdentity . collapseRange . apply . splitBoardingPass <$> boardingPasses
    return $ missing ordered
