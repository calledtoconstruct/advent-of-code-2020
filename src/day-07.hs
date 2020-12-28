import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Either ( fromRight )

data Rule = Rule {
    ruleColor :: Text.Text,
    ruleAllows :: [Text.Text],
    ruleRequires :: [Requirement]
} deriving (Show)

data Requirement = Requirement {
    requirementCount :: Int,
    requirementColor :: Text.Text
} deriving (Show)

sampleRules :: [Rule]
sampleRules = [
    Rule { 
        ruleColor = Text.pack "light red",
        ruleAllows = [
            Text.pack "bright white",
            Text.pack "muted yellow"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "bright white",
                requirementCount = 1
            },
            Requirement {
                requirementColor = Text.pack "muted yellow",
                requirementCount = 2
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "dark orange",
        ruleAllows = [
            Text.pack "bright white",
            Text.pack "muted yellow"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "bright white",
                requirementCount = 3
            },
            Requirement {
                requirementColor = Text.pack "muted yellow",
                requirementCount = 4
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "bright white",
        ruleAllows = [
            Text.pack "shiny gold"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "shiny gold",
                requirementCount = 1
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "muted yellow",
        ruleAllows = [
            Text.pack "shiny gold",
            Text.pack "faded blue"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "shiny gold",
                requirementCount = 2
            },
            Requirement {
                requirementColor = Text.pack "faded blue",
                requirementCount = 9
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "shiny gold",
        ruleAllows = [
            Text.pack "dark olive",
            Text.pack "vibrant plum"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "dark olive",
                requirementCount = 1
            },
            Requirement {
                requirementColor = Text.pack "vibrant plum",
                requirementCount = 2
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "dark olive",
        ruleAllows = [
            Text.pack "faded blue",
            Text.pack "dotted black"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "faded blue",
                requirementCount = 3
            },
            Requirement {
                requirementColor = Text.pack "dotted black",
                requirementCount = 4
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "vibrant plum",
        ruleAllows = [
            Text.pack "faded blue",
            Text.pack "dotted black"
        ],
        ruleRequires = [
            Requirement {
                requirementColor = Text.pack "faded blue",
                requirementCount = 5
            },
            Requirement {
                requirementColor = Text.pack "dotted black",
                requirementCount = 6
            }
        ]
    }, Rule { 
        ruleColor = Text.pack "faded blue",
        ruleAllows = [],
        ruleRequires = []
    }, Rule { 
        ruleColor = Text.pack "dotted black",
        ruleAllows = [],
        ruleRequires = []
    }]

readAllowed :: [Text.Text] -> [Text.Text] -> [Text.Text]
readAllowed allowed tokens
    | null tokens = allowed
    | quantity == Text.pack "no" = []
    | otherwise = readAllowed (color': allowed) $ drop 4 tokens
    where quantity = head tokens
          color' = concatColor $ take 2 $ drop 1 tokens

readRequired :: [Requirement] -> [Text.Text] -> [Requirement]
readRequired required tokens
    | null tokens = required
    | quantity == Text.pack "no" = []
    | otherwise = readRequired (requirement: required) $ drop 4 tokens
    where quantity = head tokens
          requirement = Requirement {
              requirementColor = concatColor $ take 2 $ drop 1 tokens,
              requirementCount = fst $ fromRight (0, Text.pack "") $ Text.decimal $ head tokens
          }

concatColor :: [Text.Text] -> Text.Text
concatColor = foldl (\ p c -> if Text.null p then c else Text.concat [p, Text.pack " ", c]) (Text.pack "")

readRule :: Text.Text -> Rule
readRule ruleText = Rule {
    ruleColor = concatColor $ take 2 tokens,
    ruleAllows = readAllowed [] $ drop 1 $ dropWhile (/= Text.pack "contain") tokens,
    ruleRequires = readRequired [] $ drop 1 $ dropWhile (/= Text.pack "contain") tokens
}
    where tokens = Text.split (== ' ') ruleText

rulesAllowing :: [Rule] -> [Rule] -> Text.Text -> [Rule]
rulesAllowing rules matched color'
    | null filtered = matched
    | otherwise = foldl (rulesAllowing rules) (matched ++ filtered) filteredColors
    where allowedBy = filter (\ rule -> color' `elem` ruleAllows rule) rules
          filtered = filter (\ rule -> not $ any (\ rule' -> ruleColor rule == ruleColor rule') matched) allowedBy
          filteredColors = ruleColor <$> filtered

partOneSample :: Int
partOneSample = length $ rulesAllowing sampleRules [] (Text.pack "shiny gold")

loadData :: IO [Rule]
loadData = do
    ruleText <- Text.lines <$> Text.readFile "./data/data-day-07.txt"
    return $ fmap readRule ruleText

partOne :: IO Int
partOne = do
    rules <- loadData
    return $ length $ rulesAllowing rules [] $ Text.pack "shiny gold"

-- Part Two

childCount :: [Rule] -> Requirement -> Int
childCount rules child = count * x
    where count = requirementCount child
          x = required rules $ requirementColor child

required :: [Rule] -> Text.Text -> Int
required rules color
    | null children = 1
    | otherwise  = (+) 1 $ sum $ childCount rules <$> children
    where currentRule = head $ filter ((== color) . ruleColor) rules
          children = ruleRequires currentRule

partTwoSample :: Int
partTwoSample = flip (-) 1 $ required sampleRules $ Text.pack "shiny gold"

partTwo :: IO Int
partTwo = do
    rules <- loadData
    return $ flip (-) 1 $ required rules $ Text.pack "shiny gold"