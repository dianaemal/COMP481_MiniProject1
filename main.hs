import Text.XHtml (sample)
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
