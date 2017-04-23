{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Data.Char (isSpace)
import Text.RawString.QQ
import Text.Trifecta hiding (Result)
import qualified Text.Trifecta as Tri (Result(Success, Failure))
import Control.Monad (replicateM)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as Txt (strip, pack, Text)
import Text.Printf (printf)
import Control.Applicative

-- 5. Write a parser for a log file format and sum the time spent in
-- each activity. Additionally, provide an alternative aggregation
-- of the data that provides average time spent per activity per day.
-- The format supports the use of comments which your parser
-- will have to ignore. The # characters followed by a date mark
-- the beginning of a particular day.
-- Log format example:

logBook = [r|-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
-- extra comment
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
-- useless comment
21:00 Dinner
21:15 Read
22:00 Sleep

|]

-- You are to derive a reasonable datatype for representing this
-- data yourself. For bonus points, make this bi-directional by
-- making a Show representation for the datatype which matches
-- the format you are parsing. Then write a generator for this data
-- using QuickCheck’s Gen and see if you can break your parser
-- with QuickCheck.

newtype Log = Log [LogDay] deriving Eq

data LogDay = LogDay LogDate [LogEntry] deriving Eq

data LogDate = LogDate LogDateMonth LogDateDay LogDateYear deriving Eq

data LogEntry = LogEntry EntryTime EntryText deriving Eq

data EntryTime = EntryTime EntryHours EntryMinutes deriving Eq

newtype LogDateDay = LogDateDay {getDay :: Integer} deriving Eq

newtype LogDateMonth = LogDateMonth {getMonth :: Integer} deriving Eq

newtype LogDateYear = LogDateYear {getYear :: Integer} deriving Eq

newtype EntryText = EntryText {getText :: String} deriving Eq

newtype EntryHours = EntryHours {getHours :: Integer} deriving Eq

newtype EntryMinutes = EntryMinutes {getMinutes :: Integer} deriving Eq

newtype PrintResult a = PrintResult (Tri.Result a)

-- Show instances

instance Show Log where
  show (Log logDays) = foldMap show logDays

instance Show LogDay where
  show (LogDay date entries) = concat [show date, foldMap show entries, "\n"]

instance Show LogEntry where
  show (LogEntry time text) = concat [show time, " ", getText text, "\n"]

instance Show EntryTime where
  show (EntryTime hrs mins) = concat [printf "%02d" $ getHours hrs
                                      , ":"
                                      , printf "%02d" $ getMinutes mins
                                     ]

instance Show LogDate where
  show (LogDate month day year) = concat [ "# "
                                         , printf "%04d" $ getYear year
                                         , "-"
                                         , printf "%02d" $ getDay day
                                         , "-"
                                         , printf "%02d" $ getMonth month
                                         , "\n"
                                         ]

instance Show a => Show (PrintResult a) where
  show (PrintResult (Tri.Success a)) = show a
  show (PrintResult (Tri.Failure _)) = ""

instance Show EntryText where
  show = show . getText

-- Arbitrary instances

instance Arbitrary Log where
  arbitrary = Log <$> listOf (arbitrary :: Gen LogDay)

instance Arbitrary LogDay where
  arbitrary = liftA2 LogDay (arbitrary :: Gen LogDate)
                            $ listOf (arbitrary :: Gen LogEntry)

instance Arbitrary LogDateMonth where
  arbitrary = LogDateMonth <$> elements [1..12]

instance Arbitrary LogDateDay where
  arbitrary = LogDateDay <$> elements [1..31]

instance Arbitrary LogDateYear where
  arbitrary = LogDateYear <$> elements [2001..7999]

instance Arbitrary LogDate where
  arbitrary = do
    month <- arbitrary
    day <- arbitrary
    year <- arbitrary
    return $ LogDate month day year

instance Arbitrary LogEntry where
  arbitrary = do
    time <- arbitrary
    text <- arbitrary
    return $ LogEntry time text

instance Arbitrary EntryHours where
  arbitrary = EntryHours <$> elements [0..23]

instance Arbitrary EntryMinutes where
  arbitrary = EntryMinutes <$> elements [0..59]

instance Arbitrary EntryTime where
  arbitrary = do
    hours <- arbitrary
    minutes <- arbitrary
    return $ EntryTime hours minutes

instance Arbitrary EntryText where
  arbitrary = EntryText <$> validEntryText (listOf $ elements "help me")

-- Functions

validEntryText :: Gen String -> Gen String
validEntryText = flip suchThat $ liftA2 (&&) notBlank noTrailingWhitespace

noTrailingWhitespace :: String -> Bool
noTrailingWhitespace = not . isSpace . last

notBlank :: String -> Bool
notBlank = any $ not . isSpace

parseSomeDigits :: Int -> Parser Integer
parseSomeDigits num = read <$> replicateM num digit

comment :: Parser String
comment = string "--" >> some (noneOf "\n") <* char '\n'

skipComment :: Parser ()
skipComment = skipMany comment

skipBlank :: Parser ()
skipBlank = skipMany $ oneOf " \t"

endOfEntryText :: Parser (Either () String)
endOfEntryText = Left <$> skipBlank
                 >> Right <$> string "\n"
                 <|> Right <$> comment
                 <|> Left <$> eof

parseEntryTime :: Parser EntryTime
parseEntryTime = do
  skipComment
  hours <- parseSomeDigits 2
  char ':'
  minutes <- parseSomeDigits 2
  return $ EntryTime (EntryHours hours) (EntryMinutes minutes)

parseEntryText :: Parser EntryText
parseEntryText = EntryText <$> manyTill anyChar (try endOfEntryText)

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  time <- parseEntryTime
  space
  text <- parseEntryText
  return $ LogEntry time text

parseLogEntries :: Parser [LogEntry]
parseLogEntries = many parseLogEntry

parseLogDate :: Parser LogDate
parseLogDate = do
  skipComment
  string "# "
  year <- parseSomeDigits 4
  char '-'
  day <- parseSomeDigits 2
  char '-'
  month <- parseSomeDigits 2
  spaces
  return $ LogDate (LogDateMonth month) (LogDateDay day) (LogDateYear year)

parseLogDay :: Parser LogDay
parseLogDay = do
  spaces
  date <- parseLogDate
  entries <- parseLogEntries
  spaces
  return $ LogDay date entries

parseLog :: Parser Log
parseLog = Log <$> some parseLogDay

-- Test

logTest :: IO ()
logTest = hspec $
  describe "logbook parsing" $ do
    it "creates a Log" $ do
      let expectedLog = Log
                        [LogDay
                         (LogDate (LogDateMonth 5) (LogDateDay 2) (LogDateYear 2025))
                         [ LogEntry (EntryTime (EntryHours 8) (EntryMinutes 0)) $ EntryText "Breakfast"
                         , LogEntry (EntryTime (EntryHours 9) (EntryMinutes 0)) $ EntryText "Sanitizing moisture collector"
                         , LogEntry (EntryTime (EntryHours 11) (EntryMinutes 0)) $ EntryText "Exercising in high-grav gym"
                         , LogEntry (EntryTime (EntryHours 12) (EntryMinutes 0)) $ EntryText "Lunch"
                         , LogEntry (EntryTime (EntryHours 13) (EntryMinutes 0)) $ EntryText "Programming"
                         , LogEntry (EntryTime (EntryHours 17) (EntryMinutes 0)) $ EntryText "Commuting home in rover"
                         , LogEntry (EntryTime (EntryHours 17) (EntryMinutes 30)) $ EntryText "R&R"
                         , LogEntry (EntryTime (EntryHours 19) (EntryMinutes 0)) $ EntryText "Dinner"
                         , LogEntry (EntryTime (EntryHours 21) (EntryMinutes 0)) $ EntryText "Shower"
                         , LogEntry (EntryTime (EntryHours 21) (EntryMinutes 15)) $ EntryText "Read"
                         , LogEntry (EntryTime (EntryHours 22) (EntryMinutes 0)) $ EntryText "Sleep"
                         ]

                        , LogDay
                          (LogDate (LogDateMonth 7) (LogDateDay 2) (LogDateYear 2025))
                          [ LogEntry (EntryTime (EntryHours 8) (EntryMinutes 0)) $ EntryText "Breakfast"
                          , LogEntry (EntryTime (EntryHours 9) (EntryMinutes 0)) $ EntryText "Bumped head, passed out"
                          , LogEntry (EntryTime (EntryHours 13) (EntryMinutes 36)) $ EntryText "Wake up, headache"
                          , LogEntry (EntryTime (EntryHours 13) (EntryMinutes 37)) $ EntryText "Go to medbay"
                          , LogEntry (EntryTime (EntryHours 13) (EntryMinutes 40)) $ EntryText "Patch self up"
                          , LogEntry (EntryTime (EntryHours 13) (EntryMinutes 45)) $ EntryText "Commute home for rest"
                          , LogEntry (EntryTime (EntryHours 14) (EntryMinutes 15)) $ EntryText "Read"
                          , LogEntry (EntryTime (EntryHours 21) (EntryMinutes 0)) $ EntryText "Dinner"
                          , LogEntry (EntryTime (EntryHours 21) (EntryMinutes 15)) $ EntryText "Read"
                          , LogEntry (EntryTime (EntryHours 22) (EntryMinutes 0)) $ EntryText "Sleep"
                          ]
                        ]

      shouldBe (eitherSuccess $ parseString parseLog mempty logBook)
               (Right expectedLog)

    it "roundtrip: parsing then printing logs" $
      property $ \log -> collect log $ logRoundTripProp $ show (log :: Log)

logRoundTrip :: String -> String
logRoundTrip = show . PrintResult . parseString parseLog mempty

logRoundTripProp :: String -> Property
logRoundTripProp = liftA2 (===) (strip . logRoundTrip) strip

strip :: String -> Txt.Text
strip = Txt.strip . Txt.pack

eitherSuccess :: Tri.Result a -> Either ErrInfo a
eitherSuccess (Tri.Success a) = Right a
eitherSuccess (Tri.Failure a) = Left a

instance Eq ErrInfo where
  (==) err1 err2 = show err1 == show err2
