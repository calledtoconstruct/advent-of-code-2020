import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Rule = Rule {
    color :: Text.Text,
    allowed :: [Text.Text]
} deriving (Show)

sampleRules :: [Rule]
sampleRules = [
    Rule { 
        color = Text.pack "light red",
        allowed = [
            Text.pack "bright white",
            Text.pack "muted yellow"
        ]
    }, Rule { 
        color = Text.pack "dark orange",
        allowed = [
            Text.pack "bright white",
            Text.pack "muted yellow"
        ]
    }, Rule { 
        color = Text.pack "bright white",
        allowed = [
            Text.pack "shiny gold"
        ]
    }, Rule { 
        color = Text.pack "muted yellow",
        allowed = [
            Text.pack "shiny gold",
            Text.pack "faded blue"
        ]
    }, Rule { 
        color = Text.pack "shiny gold",
        allowed = [
            Text.pack "dark olive",
            Text.pack "vibrant plum"
        ]
    }, Rule { 
        color = Text.pack "dark olive",
        allowed = [
            Text.pack "faded blue",
            Text.pack "dotted black"
        ]
    }, Rule { 
        color = Text.pack "vibrant plum",
        allowed = [
            Text.pack "faded blue",
            Text.pack "dotted black"
        ]
    }, Rule { 
        color = Text.pack "faded blue",
        allowed = []
    }, Rule { 
        color = Text.pack "dotted black",
        allowed = []
    }]

rulesAllowing :: [Rule] -> [Rule] -> Text.Text -> [Rule]
rulesAllowing rules matched color'
    | null filtered = matched
    | otherwise = foldl (rulesAllowing rules) (matched ++ filtered) filteredColors
    where allowedBy = filter (\ rule -> color' `elem` allowed rule) rules
          filtered = filter (\ rule -> not $ any (\ rule' -> color rule == color rule') matched) allowedBy
          filteredColors = color <$> filtered

readAllowed :: [Text.Text] -> [Text.Text] -> [Text.Text]
readAllowed allowed tokens
    | null tokens = allowed
    | quantity == Text.pack "no" = []
    | otherwise = readAllowed (color': allowed) $ drop 4 tokens
    where quantity = head tokens
          color' = concatColor $ take 2 $ drop 1 tokens

concatColor :: [Text.Text] -> Text.Text
concatColor = foldl (\ p c -> if Text.null p then c else Text.concat [p, Text.pack " ", c]) (Text.pack "")

readRule :: Text.Text -> Rule
readRule ruleText = Rule {
    color = concatColor $ take 2 tokens,
    allowed = readAllowed [] $ drop 1 $ dropWhile (/= Text.pack "contain") tokens
}
    where tokens = Text.split (== ' ') ruleText

partOneSample :: [Rule]
partOneSample = rulesAllowing sampleRules [] (Text.pack "shiny gold")

loadData :: IO [Rule]
loadData = do
    ruleText <- Text.lines <$> Text.readFile "./data/data-day-07.txt"
    return $ fmap readRule ruleText

partOne :: IO Int
partOne = do
    rules <- loadData
    return $ length $ rulesAllowing rules [] (Text.pack "shiny gold")
