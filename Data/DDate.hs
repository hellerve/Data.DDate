module Data.DDate(DDateTime,
                  DDate,
                  Yold,
                  Season,
                  Day,
                  weekdays,
                  seasons,
                  fluxes,
                  holydays,
                  yold,
                  convertSeason,
                  convertDay,
                  holyday,
                  convertDateTime,
                  convertDate,
                  ddateToDDateTime,
                  ddateTimeToDDate,
                 )
  where
import Data.Dates (DateTime(DateTime), getCurrentDateTime)
import Data.Time.Calendar (isLeapYear)

-- hack to make holyday generic
data DDatable = DT DDateTime
              | D DDate

-- |The DDateTime data type. It consists of a Yold, a Season, a Day
-- |and three integers that signify the time of day.
data DDateTime = DDateTime {
  year :: Yold,
  season :: Season,
  day :: Day,
  hour :: Int,
  minute :: Int,
  second :: Int
}
  deriving (Eq, Ord)

instance Show DDateTime where
  show (DDateTime y s d h m sec) =
      show (DDate y s d) ++ ", " ++ pad(show h) ++ ":" ++
      pad (show m) ++ ":" ++ pad (show sec)
    where pad s = replicate (2 - (length s)) '0' ++ s

-- |The DDateTime data type. It consists of a Yold, a Season and a Day.
data DDate = DDate {
  y :: Yold,
  s :: Season,
  d :: Day
}

instance Show DDate where
  show (DDate y s d@(Day dayval)) =
    show d ++ ", " ++ show s ++ " the " ++ dayofmonth dayval ++
    ", " ++ show y ++ " YOLD"

-- |The Yold type. It's just an Int.
type Yold = Int

-- |The Season data type. It's just a wrapped Int.
data Season = Season Int
  deriving (Eq, Ord)

instance Show Season where
  show (Season s) = seasons !! s

-- |The Day type. It's just a wrapped Int.
data Day = Day Int
  deriving (Eq, Ord)

instance Show Day where
  show (Day d) = weekdays !! (weekday d)

-- |A function returning the names of the days of the week.
weekdays :: [String]
weekdays = [ "Sweetmorn"
           , "Boomtime"
           , "Pungenday"
           , "Prickle-Prickle"
           , "Setting Orange"
           ]

-- |A function returning the names of the seasons.
seasons :: [String]
seasons = [ "Chaos"
          , "Discord"
          , "Confusion"
          , "Bureacracy"
          , "The Aftermath"
          ]

-- |A function returning the names of the fluxes.
fluxes :: [String]
fluxes = [ "Chaoflux"
         , "Discoflux"
         , "Confuflux"
         , "Bureflux"
         , "Afflux"
         ]

-- |A function returning the names of the holydays.
holydays :: [String]
holydays = [ "Mungday"
           , "Mojoday"
           , "Syaday"
           , "Zaraday"
           , "Maladay"
           ]

-- | Converts an Int to a Season (takes the day of the year).
convertSeason :: Int -> Season
convertSeason x = Season $ quot x 73

-- | Converts an Int to it's weekday (also Int).
weekday :: Int -> Int
weekday x = rem x 5

-- | Converts an Int to a Day (takes the day of the year).
convertDay :: Int -> Day
convertDay x = Day $ rem x 73

dayofmonth :: Int -> String
dayofmonth x =
        if day > 3 && day < 21
            then sday ++ "th"
            else case mod day 10 of
                1 -> sday ++ "st"
                2 -> sday ++ "nd"
                3 -> sday ++ "rd"
                _ -> sday ++ "th"
    where day = rem x 73
          sday = show $ day

-- | Converts an Int to a YOLD (takes a regular year).
yold :: Int -> Yold
yold year = (year + 1166)

holyday' :: Int -> String
holyday' day
    | rem day 73 == 50 =
            printHoly holydays day
    | rem day 73 == 5 =
            printHoly fluxes day
    | otherwise = ""
    where printHoly l x = "\nCelebrate " ++ l !! quot x 73 ++ "!"

-- | Takes a DDate or DDateTime instance and returns
-- | the appropriate Holyday (or an empty string).
holyday :: DDatable -> String
holyday (DT (DDateTime _ _ (Day d) _ _ _)) = holyday' d
holyday (D (DDate _ _ (Day d))) = holyday' d

monthdays :: [Int]
monthdays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- | Upgrades a DateTime to a DDateTime.
convertDateTime :: DateTime -> DDateTime
convertDateTime dt@(DateTime y m d h min s) =
  let totalDays = d + sum (take (m - 1) monthdays)
  in DDateTime (yold y) (convertSeason totalDays) (convertDay totalDays) h min s

-- | Upgrades a DateTime to a DDate.
convertDate :: DateTime -> DDate
convertDate dt@(DateTime y m d _ _ _) =
  let totalDays = d + sum (take (m - 1) monthdays)
  in DDate (yold y) (convertSeason totalDays) (convertDay totalDays)

-- | Converts a DDate instance to a DDateTime instance
-- | with the time set to midnight.
ddateToDDateTime :: DDate -> DDateTime
ddateToDDateTime (DDate y s d) = DDateTime y s d 0 0 0

-- | Converts a DDateTime instance to a DDate instance.
ddateTimeToDDate :: DDateTime -> DDate
ddateTimeToDDate (DDateTime y s d _ _ _) = DDate y s d

-- | Gets the current DDateTime.
currentDDateTime :: IO DDateTime
currentDDateTime = do
  dt <- getCurrentDateTime
  return $ convertDateTime dt
