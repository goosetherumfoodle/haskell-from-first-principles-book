{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Text.Trifecta
import Control.Applicative

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser NumberOrString
parseNos = skipMany (oneOf "\n") >>
           (Left <$> integer)
           <|> (Right <$> some letter)

main :: IO ()
main = print $ parseString (some (token parseNos)) mempty eitherOr
