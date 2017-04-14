{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

type NumberOrString = Either Integer String


eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n")
           >> (Left <$> integer)
           <|> (Right <$> some letter)

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
parseDigit = foldr (<|>) (char '0') (char <$> "123456789")

base10Integer :: Parser Integer
base10Integer = some parseDigit >>= \digits -> return $ read digits

main :: IO ()
main = do
  print $ parseString (some (token parseNos)) mempty eitherOr
  print $ parseString parseSemVer mempty "1.5.10"
