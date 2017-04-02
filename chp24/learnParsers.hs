module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

-- pg 891
-- ex: Parsing Practice

-- 1. There’s a combinator that’ll let us mark that we expect an input
-- stream to be “finished” at a particular point in our parser. In
-- the parsers library this is simply called eof (end-of-file) and is
-- in the Text.Parser.Combinators module. See if you can make the
-- one and oneTwo parsers fail because they didn’t exhaust the input
-- stream!

testParse' :: Show a => Parser a -> IO ()
testParse' p = print $ parseString p mempty "123"

oneOnly = one >> eof

oneTwoOnly = oneTwo >> eof

-- 2. Use string to make a Parser that parses “1”, “12”, and “123” out of
-- the example input respectively. Try combining it with stop too.
-- That is, a single parser should be able to parse all three of those
-- strings.

flexParse = testParse' $ choice [string "123", string "12", string "1"]

-- 3. Try writing a Parser that does what string does, but using char.

-- string' :: (CharParsing m, Monad m) => String -> m Char
-- string' s = foldr ((>>) . char) mempty s

string' :: (Monad m, CharParsing m) => [Char] -> m [Char]
string' s = sequence $ map char s

-- pg 899

-- Prelude> parseString (integer >> eof) mempty "123"
-- Success ()

-- You may have already deduced why it returns () as a Success
-- result here; it’s consumed all the input but there is no result to re-
-- turn from having done so. The result Success () tells you the parse
-- was successful and consumed the entire input, so there’s nothing to
-- return.

-- What we want you to try now is rewriting the final example so
-- it returns the integer that it parsed instead of Success (). It should
-- return the integer successfully when it receives an input with an
-- integer followed by an EOF and fail in all other cases:

-- Prelude> parseString (yourFuncHere) mempty "123"
-- Success 123

-- Prelude> parseString (yourFuncHere) mempty "123abc"
-- Failure (interactive):1:4: error: expected: digit,
-- end of input
-- 123abc<EOF>

returnInt = parseString (integer <* eof) mempty

returnInt' = parseString retFirst mempty

retFirst = do
  foundInt <- integer
  eof
  return foundInt

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
