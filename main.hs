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



-- Formatting and queries 

 
-- Pad a string with spaces on the right so columns line up.
-- If the text is already longer than the width, it is left as is.
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '
 
-- Pad on the left, used to right-align numbers.
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s
 
-- Show a money amount with exactly 2 decimals (12.5 -> "12.50").
-- This also hides floating-point noise like 508.79999999999995.
formatMoney :: Double -> String
formatMoney = printf "%.2f"


-- transactions as an aligned table:

-- Column widths come from the data, so long notes or categories
-- never break the layout.
formatTransactions :: [Transaction] -> String
formatTransactions [] = "(no transactions)\n"
formatTransactions txs =
  unlines (header : divider : map formatRow txs)
  where
    dateW   = maximum (length "Date"     : map (length . tDate) txs)
    catW    = maximum (length "Category" : map (length . tCategory) txs)
    amtW    = maximum (length "Amount"   : map (length . formatMoney . tAmount) txs)
 
    header  = padRight dateW "Date" ++ " | "
           ++ padRight catW "Category" ++ " | "
           ++ padLeft amtW "Amount" ++ " | Note"
    divider = replicate (length header + 4) '-'
 
    formatRow t = padRight dateW (tDate t) ++ " | "
               ++ padRight catW (tCategory t) ++ " | "
               ++ padLeft amtW (formatMoney (tAmount t)) ++ " | "
               ++ tNote t
 
-- Category totals and counts as one table.
-- Uses a list comprehension to pair each total with its count.
formatSummary :: [Transaction] -> String
formatSummary txs =
  unlines (header : divider : rows ++ [divider, footer])
  where
    totals = totalByCategory txs
    counts = countByCategory txs
    -- Both lists come from groupByCategory, so they are in the same order
    combined = [ (cat, total, n) | ((cat, total), (_, n)) <- zip totals counts ]
 
    catW = maximum (length "Category" : map (length . fst) totals)
    amtW = maximum (length "Total" : map (length . formatMoney . snd) totals
                    ++ [length (formatMoney (overallTotal txs))])
 
    header  = padRight catW "Category" ++ " | " ++ padLeft amtW "Total" ++ " | Count"
    divider = replicate (length header) '-'
    rows    = [ padRight catW cat ++ " | " ++ padLeft amtW (formatMoney total)
                ++ " | " ++ show n
              | (cat, total, n) <- combined ]
    footer  = padRight catW "ALL" ++ " | " ++ padLeft amtW (formatMoney (overallTotal txs))
              ++ " | " ++ show (length txs)
 
-- Print a heading followed by a table, with a blank line after.
printSection :: String -> String -> IO ()
printSection title body = do
  putStrLn ("== " ++ title ++ " ==")
  putStrLn body



 
-- combine the filters and summaries to answer real questions:
main :: IO ()
main = do
  let allTxs     = sampleTransactions
      bigFood    = filterByMinAmount 10 (filterByCategory "Food" allTxs)
      bigTravel  = filterByMinAmount 100 (filterByCategory "Travel" allTxs)
      sameDay    = filterByDate "2023-12-27" allTxs
 
  printSection "All transactions" (formatTransactions allTxs)
  printSection "Food transactions of 10.00 or more" (formatTransactions bigFood)
  printSection "Travel transactions of 100.00 or more" (formatTransactions bigTravel)
  printSection "Transactions on 2023-12-27" (formatTransactions sameDay)
  printSection "Totals and counts by category" (formatSummary allTxs)
 
  putStrLn "== Largest transaction =="
  case largestTransaction allTxs of
    Nothing -> putStrLn "(no transactions)"
    Just t  -> putStr (formatTransactions [t])
 