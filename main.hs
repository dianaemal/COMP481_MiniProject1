-- Won't compile until filterbyMinAmount and mapping functions are done.
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
  ]

filterByCategory :: String -> [Transaction] -> [Transaction]
filterByCategory cat trsctn = filter (\t -> tCategory t == cat) trsctn 

-- Add filterbyMin

-- Summarize data by category
{- 1: Sort by category 
   2: Grouped by category (ensure all categories are grouped with eachother)
   3: Pair category name with the sum of it's group (output is a tuple)
-}

formatTransactions :: [Transaction] -> String
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
  print (totalByCategory sampleTransactions)
