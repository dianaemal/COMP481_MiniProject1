import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)

data Transaction = Transaction
  { tDate     :: String
  , tCategory :: String
  , tAmount   :: Double
  , tNote     :: String
  } deriving (Show, Eq)

sampleTransactions :: [ Transaction ]
sampleTransactions =
  [ Transaction "2024-03-01" "Food"   12.50  "Lunch at cafe"
  , Transaction "2024-03-02" "Travel" 120.00 "Taxi and train"
  , Transaction "2024-03-04" "Food"   7.80   "Coffee"
  , Transaction "2024-01-02" "Living"   100.00   "AirBNB"
  , Transaction "2024-12-22" "Food"   63.32   "Team Dinner"
  , Transaction "2025-12-26" "Travel"   644.92   "Flight to Paris"
  , Transaction "2023-12-27" "Travel"   24.02   "Uber to conference"
  , Transaction "2023-12-27" "Travel"   34.52   "Uber from conference"
  , Transaction "2023-12-28" "Food"   62.45   "5 star meal in Paris"
  , Transaction "2023-01-31" "Travel"   544.02   "Flight back home"
  , Transaction "2024-01-03" "Food"   362.73   "Team dinner"
  ]

--Filters by Category
filterByCategory :: String -> [Transaction] -> [Transaction]
filterByCategory cat trsctn = filter (\t -> tCategory t == cat) trsctn 

--Filters by Minimum Amount
filterByMinAmount :: Double -> [Transaction] -> [Transaction]
filterByMinAmount amount trsctn = filter (\t -> tAmount t >= amount) trsctn

--Filters by Date
filterByDate :: String -> [Transaction] -> [Transaction]
filterByDate date trsctn = filter (\t -> tDate t == date) trsctn

-- Explicit recursion + pattern matching
sumAmounts :: [Transaction] -> Double
sumAmounts []     = 0
sumAmounts (t:ts) = tAmount t + sumAmounts ts

-- Sort 
groupByCategory :: [Transaction] -> [[Transaction]]
groupByCategory txs = groupBy ((==) `on` tCategory) sorted
  where sorted = sortBy (comparing tCategory) txs

-- Total per category
totalByCategory :: [Transaction] -> [(String, Double)]
totalByCategory txs = map summarize (groupByCategory txs)
  where summarize grp = (tCategory (head grp), sumAmounts grp)

-- Number of transactions per category
countByCategory :: [Transaction] -> [(String, Int)]
countByCategory txs = map summarize (groupByCategory txs)
  where summarize grp = (tCategory (head grp), length grp)

-- Largest transaction
largestTransaction :: [Transaction] -> Maybe Transaction
largestTransaction [] = Nothing
largestTransaction txs = Just (maximumBy (comparing tAmount) txs)

-- Overall total using foldr
overallTotal :: [Transaction] -> Double
overallTotal = foldr (\t acc -> tAmount t + acc) 0

-- Formatted PDF CODE add yours above this! 

{- formatTransactions :: [Transaction] -> String
formatTransactions txs =
  unlines (header : map formatRow txs)
  where
    header = "Date       | Category   | Amount  | Note"
    formatRow t = tDate t ++ " | "
               ++ tCategory t ++ " | "
               ++ show (tAmount t) ++ " | "
               ++ tNote t

main :: IO ()
main = do
  let bigFood = filterByMinAmount 10 (filterByCategory "Food" sampleTransactions)
  putStrLn "Food transactions over 10:"
  putStrLn (formatTransactions bigFood)
  putStrLn "Totals by category:"
  print (totalByCategory sampleTransactions) -}
