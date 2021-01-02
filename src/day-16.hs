import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either (fromRight)

data Range = Range {
    lower :: Int,
    upper :: Int
} deriving (Show)

data Rule = Rule {
    name :: Text.Text,
    ranges :: [Range]
} deriving (Show)

newtype Ticket = Ticket {
    values :: [Int]
} deriving (Show, Eq)

data ReadState = Rules | OwnTicketLabel | OwnTicket | NearbyTicketLabel | NearbyTicket deriving (Eq)

initialReadState :: ReadState
initialReadState = Rules

nextState :: ReadState -> Text.Text -> ReadState
nextState readState input
    | input == Text.pack "" = case readState of
        Rules -> OwnTicketLabel
        OwnTicket -> NearbyTicketLabel
    | Text.last input == ':' = case readState of
        OwnTicketLabel -> OwnTicket
        NearbyTicketLabel -> NearbyTicket

readRanges :: Text.Text -> [Range]
readRanges input = [Range { lower = firstLower, upper = firstUpper }, Range { lower = secondLower, upper = secondUpper }]
    where [firstRangeText, secondRangeText] = Text.splitOn (Text.pack " or ") input
          [firstLowerText, firstUpperText] = Text.splitOn (Text.pack "-") firstRangeText
          firstLower = fst $ fromRight (0, Text.pack "") $ Text.decimal firstLowerText
          firstUpper = fst $ fromRight (0, Text.pack "") $ Text.decimal firstUpperText
          [secondLowerText, secondUpperText] = Text.splitOn (Text.pack "-") secondRangeText
          secondLower = fst $ fromRight (0, Text.pack "") $ Text.decimal secondLowerText
          secondUpper = fst $ fromRight (0, Text.pack "") $ Text.decimal secondUpperText

readRule :: Text.Text -> [Rule] -> [Rule]
readRule input rules = Rule { name = ruleName, ranges = ruleRanges } : rules
    where ruleName = Text.takeWhile (/= ':') input
          ruleRanges = readRanges $ Text.strip $ Text.tail $ Text.dropWhile (/= ':') input

readTicket :: Text.Text -> [Ticket] -> [Ticket]
readTicket input existingTickets = newTicket: existingTickets
    where ticketsText = Text.splitOn (Text.pack ",") input
          newTicket = Ticket { values = fst . fromRight (0, Text.pack "") . Text.decimal <$> ticketsText }

readData :: ([Rule], [Ticket], ReadState) -> [Text.Text] -> ([Rule], [Ticket])
readData (rules, tickets, readState) instructions
    | null instructions = (rules, tickets)
    | readState == Rules && Text.null input = readData (rules, tickets, newState) (tail instructions)
    | readState == Rules = readData (readRule input rules, tickets, readState) (tail instructions)
    | readState == OwnTicketLabel = readData (rules, tickets, newState) (tail instructions)
    | readState == NearbyTicketLabel = readData (rules, tickets, newState) (tail instructions)
    | readState == OwnTicket && Text.null input = readData (rules, tickets, newState) (tail instructions)
    | readState == OwnTicket = readData (rules, readTicket input tickets, readState) (tail instructions)
    | readState == NearbyTicket && Text.null input = readData (rules, tickets, newState) (tail instructions)
    | readState == NearbyTicket = readData (rules, readTicket input tickets, readState) (tail instructions)
    where newState = nextState readState input
          input = head instructions

ticketNumbers :: [Ticket] -> [Int]
ticketNumbers tickets = concat $ values <$> tickets

applyRange :: Int -> Range -> Bool 
applyRange ticketNumber range = lower range <= ticketNumber && ticketNumber <= upper range

applyRule :: Int -> Rule -> Bool
applyRule ticketNumber rule = or $ applyRange ticketNumber <$> ranges rule

applyRules :: [Rule] -> Int -> Bool
applyRules rules ticketNumber = or $ applyRule ticketNumber <$> rules

invalid :: [Rule] -> [Ticket] -> [Int]
invalid rules tickets = filter (not . applyRules rules) $ concat  $ values <$> tickets

errorRate :: [Int] -> Int
errorRate = sum

partOneSampleData :: [Text.Text]
partOneSampleData = [
    Text.pack "class: 1-3 or 5-7",
    Text.pack "row: 6-11 or 33-44",
    Text.pack "seat: 13-40 or 45-50",
    Text.pack "",
    Text.pack "your ticket:",
    Text.pack "7,1,14",
    Text.pack "",
    Text.pack "nearby tickets:",
    Text.pack "7,3,47",
    Text.pack "40,4,50",
    Text.pack "55,2,20",
    Text.pack "38,6,12"
    ]

testPartOne :: Int
testPartOne = errorRate $ invalid rules $ init ticketNumbers
    where (rules, ticketNumbers) = readData ([], [], initialReadState) partOneSampleData

loadData :: IO [Text.Text]
loadData = Text.lines <$> Text.readFile "./data/data-day-16.txt"

partOne :: IO Int
partOne = do
    trainTickets <- loadData
    let (rules, ticketNumbers) = readData ([], [], initialReadState) trainTickets
    return $ errorRate $ invalid rules $ init ticketNumbers


