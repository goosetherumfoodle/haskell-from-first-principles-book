{-# LANGUAGE OverloadedStrings #-}

module BackTracking where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)

-- run trifecta
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

-- run parsec
parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

-- run attoparsec
attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

-- doesn't backtrack
nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

-- does backtrack
tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  print "trifecta"
  trifP nobackParse "13"
  print "backstepping"
  trifP tryParse "13"
  print "annotated"
  trifP tryAnnot "13"
  print "**********************"

  print "parsec"
  parsecP nobackParse "13"
  print "backstepping"
  parsecP tryParse "13"
  print "annotated"
  parsecP tryAnnot "13"
  print "**********************"

  print "attoparsec"
  attoP nobackParse "13"
  print "backstepping"
  attoP tryParse "13"
  print "annotated"
  attoP tryAnnot "13"

-- 939
