{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative
import Control.Monad (replicateM)
import Data.Ratio ((%))
import Test.Hspec

type NumberOrString = Either Integer String


eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = skipMany newline
           >> (Left <$> integer) <|> (Right <$> some letter)

-- pg 912
-- Exercise: Try Try

-- Make a parser, using the existing fraction parser plus a new decimal
-- parser, that can parse either decimals or fractions. You’ll want to use
-- <|> from Alternative to combine the...alternative parsers. If you find
-- this too difficult, write a parser that parses straightforward integers
-- or fractions. Make a datatype that contains either an integer or a
-- rational and use that datatype as the result of the parser. Or use
-- Either.

-- from fractions.hs
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return $ numerator % denominator

type FractionOrInteger = Either Integer Rational

parseFractionOrInteger :: Parser FractionOrInteger
parseFractionOrInteger = (Right <$> try parseFraction) <|> (Left <$> try integer)

-- problems 1 and 5

-- 24.11 Chapter Exercises

-- 1. Write a parser for semantic versions as defined by http://semver.
-- org/. After making a working parser, write an Ord instance
-- for the SemVer type that obeys the specification outlined on the
-- SemVer website.

newtype Major = Major Integer deriving Show
newtype Minor = Minor Integer deriving Show
newtype Patch = Patch Integer deriving Show

data SemVer = SemVer Major Minor Patch deriving Show

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  path <- decimal
  return $ SemVer (Major major) (Minor minor) (Patch path)

-- 2. Write a parser for positive integer values. Don’t reuse the pre-
-- existing digit or integer functions, but you can use the rest of
-- the libraries we’ve shown you so far. You are not expected to
-- write a parsing library from scratch.

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = some parseDigit >>= \digits -> return $ read digits

-- 3. Extend the parser you wrote to handle negative and positive
-- integers. Try writing a new parser in terms of the one you
-- already have to do this.

base10Integer' :: Parser Integer
base10Integer' = do
  negSign <- many $ char '-'
  num <- some parseDigit
  return $ read $ concat [negSign, num]

-- 4. Write a parser for US/Canada phone numbers with varying
-- formats.

type NumberingPlanArea = Integer -- aka area code
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                 deriving (Eq, Show)

phoneSkipPhoneInitial = many $ string "1-"
phoneSkipSpacing = many $ oneOf " -()."

parseSomeDigits :: Int -> Parser Integer
parseSomeDigits num = read <$> replicateM num digit

parseNPA :: Parser Integer
parseNPA = parseSomeDigits 3

parseExchange :: Parser Integer
parseExchange = parseSomeDigits 3

parseLineNumber :: Parser Integer
parseLineNumber = parseSomeDigits 4

parsePhone :: Parser PhoneNumber
parsePhone = do
  phoneSkipPhoneInitial
  phoneSkipSpacing
  npa <- parseNPA
  phoneSkipSpacing
  exchange <- parseExchange
  phoneSkipSpacing
  lineNumber <- parseLineNumber
  eof
  return $ PhoneNumber npa exchange lineNumber

-- 5. Write a parser for a log file format and sum the time spent in
-- each activity. Additionally, provide an alternative aggregation
-- of the data that provides average time spent per activity per day.
-- The format supports the use of comments which your parser
-- will have to ignore. The # characters followed by a date mark
-- the beginning of a particular day.
-- Log format example:

logBook = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
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

data LogDay = LogDay LogDate LogEntries deriving (Show, Eq)

data LogDate = LogDate LogDateMonth LogDateDay LogDateYear deriving (Show, Eq)

data LogEntry = LogEntry EntryTime EntryText deriving (Show, Eq)

data EntryTime = EntryTime EntryHours EntryMinutes deriving (Show, Eq)

type LogEntries = [LogEntry]

type LogDateDay = Integer

type LogDateMonth = Integer

type LogDateYear = Integer

type EntryText = String

type EntryHours = Integer

type EntryMinutes = Integer

comment :: Parser [Char]
comment = string "--" >> some (noneOf "\n") <* char '\n'

skipComment :: Parser ()
skipComment = skipMany comment

skipBlank :: Parser ()
skipBlank = skipMany $ oneOf " \t"

endOfLineOrComments :: Parser String
endOfLineOrComments = skipBlank >> (string "\n" <|> comment)

parseEntryTime :: Parser EntryTime
parseEntryTime = do
  skipComment
  hours <- parseSomeDigits 2
  char ':'
  minutes <- parseSomeDigits 2
  return $ EntryTime hours minutes

parseEntryText :: Parser EntryText
parseEntryText = manyTill anyChar (try endOfLineOrComments)

parseLogEntry :: Parser LogEntry
parseLogEntry = do
  time <- parseEntryTime
  space
  text <- parseEntryText
  return $ LogEntry time text

parseLogEntries :: Parser LogEntries
parseLogEntries = some parseLogEntry

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
  return $ LogDate month day year

parseLogDay :: Parser LogDay
parseLogDay = do
  spaces
  date <- parseLogDate
  entries <- parseLogEntries
  spaces
  return $ LogDay date entries

parseLogs :: Parser [LogDay]
parseLogs = some $ parseLogDay

main :: IO ()
main = hspec $ do
  describe "logbook parsing" $ do
    it "creates list of LogDays" $ do
      shouldBe (eitherSuccess $ parseString parseLogs mempty logBook)
               (Right [LogDay
                       (LogDate 5 2 2025)
                       [ LogEntry (EntryTime 8 0) "Breakfast"
                       , LogEntry (EntryTime 9 0) "Sanitizing moisture collector"
                       , LogEntry (EntryTime 11 0) "Exercising in high-grav gym"
                       , LogEntry (EntryTime 12 0) "Lunch"
                       , LogEntry (EntryTime 13 0) "Programming"
                       , LogEntry (EntryTime 17 0) "Commuting home in rover"
                       , LogEntry (EntryTime 17 30) "R&R"
                       , LogEntry (EntryTime 19 0) "Dinner"
                       , LogEntry (EntryTime 21 0) "Shower"
                       , LogEntry (EntryTime 21 15) "Read"
                       , LogEntry (EntryTime 22 0) "Sleep"
                       ]

                      , LogDay
                        (LogDate 7 2 2025)
                        [ LogEntry (EntryTime 8 0) "Breakfast"
                        , LogEntry (EntryTime 9 0) "Bumped head, passed out"
                        , LogEntry (EntryTime 13 36) "Wake up, headache"
                        , LogEntry (EntryTime 13 37) "Go to medbay"
                        , LogEntry (EntryTime 13 40) "Patch self up"
                        , LogEntry (EntryTime 13 45) "Commute home for rest"
                        , LogEntry (EntryTime 14 15) "Read"
                        , LogEntry (EntryTime 21 0) "Dinner"
                        , LogEntry (EntryTime 21 15) "Read"
                        , LogEntry (EntryTime 22 0) "Sleep"
                        ]
                      ]
               )

eitherSuccess :: Result a -> Either ErrInfo a
eitherSuccess (Success a) = Right a
eitherSuccess (Failure a) = Left a

instance Eq ErrInfo where
  (==) err1 err2 = (show err1) == (show err2)

-- todo: generate logs
