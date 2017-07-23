{-# LANGUAGE DeriveFunctor #-}
module Example (main) where


-- Reusing the Vigenère cipher you wrote back in algebraic datatypes
-- and wrote tests for in testing, make an executable that takes a key
-- and a mode argument. If the mode is -d the executable decrypts the
-- input from stdin and writes the decrypted text to standard out. If
-- the mode is -e the executable blocks on input from standard input
-- and writes the encrypted output to stdout.

-- Consider this an opportunity to learn more about how file handles
-- and the following members of the base library work:
-- System.Environment.getArgs :: IO [String]
-- System.IO.hPutStr :: Handle -> String -> IO ()
-- System.IO.hGetChar :: Handle -> IO Char
-- System.IO.stdout :: Handle
-- System.IO.stdin :: Handle

-- Whatever OS you’re on, you’ll need to learn how to feed files as
-- input to your utility and how to redirect standard out to a file. Part
-- of the exercise is figuring this out for yourself.


import Data.Char (ord, chr, toLower)
import System.Environment (getArgs)
import System.IO (stdin, stdout, hPutStr, hGetLine, hGetContents)

newtype Key a = Key a deriving (Show, Functor)
type Keyword = Key String
data EncryptionFlag = EncryptFlag | DecryptFlag


type Plaintext = String
type Ciphertext = String
type KeyChar = Char
type PlainChar = Char
type CipherChar = Char
type PlaintextIndex = Int
type Offset = Int

main :: IO ()
main = do flag <- getFlag
          case flag of
            EncryptFlag -> mainEncrypt
            DecryptFlag -> mainDecrypt

mainEncrypt :: IO ()
mainEncrypt = do key <- foundKey
                 plainText <- foundPlainText
                 let cipherText = vigenereEncipher key plainText
                 display cipherText

mainDecrypt :: IO ()
mainDecrypt = do key <- foundKey
                 ciphertext <- foundCiphertext
                 let cipherText = vigenereDecipher key ciphertext
                 display cipherText

getFlag :: IO EncryptionFlag
getFlag = buildFlag . (!! 0) <$> getArgs

buildFlag :: String -> EncryptionFlag
buildFlag input | input == "-d" = DecryptFlag
                | input == "-D" = DecryptFlag
                | input == "-e" = EncryptFlag
                | input == "-E" = EncryptFlag

display :: String -> IO ()
display = hPutStr stdout

foundKey :: IO Keyword
foundKey = Key . (!! 1) <$> getArgs

foundCiphertext :: IO String
foundCiphertext = hGetContents stdin

foundPlainText :: IO Plaintext
foundPlainText = hGetLine stdin

vigenereEncipher :: Keyword -> Plaintext -> Ciphertext
vigenereEncipher keyword = encipherPair . (pairChars keyword)

vigenereDecipher :: Keyword -> Ciphertext -> Plaintext
vigenereDecipher keyword = decipherPair . (pairChars keyword)

pairChars :: Keyword -> String -> [(KeyChar, Char)]
pairChars (Key key) string = zip (cycle key) string

encipherPair :: [(KeyChar, Char)] -> Ciphertext
encipherPair = map combine where
  combine (a, b) = rotateCombine a b (+)

decipherPair :: [(KeyChar, Char)] -> Plaintext
decipherPair = map combine where
  combine (a, b) = rotateCombine b a (-)

rotateCombine :: Char -> Char -> (Int -> Int -> Int) -> Char
rotateCombine a b op = chr $ inflate $ (flip mod 95) $ (deflate a) `op` (deflate b) where
  deflate a = (ord a) - 31
  inflate = (31 +)

encipherChar :: Keyword -> (PlaintextIndex, PlainChar) -> CipherChar
encipherChar _ (_, ' ') = ' '
encipherChar keyword (i, pchar) = lowerAlphaASCII $ (ord pchar) + (keywordOffset keyword i)

decipherChar :: Keyword -> (PlaintextIndex, PlainChar) -> CipherChar
decipherChar _ (_, ' ') = ' '
decipherChar keyword (i, pchar) = lowerAlphaASCII $  (ord pchar) - (keywordOffset keyword i)

lowerAlphaASCII :: Int -> Char
lowerAlphaASCII = chr . (+ 97) . (flip mod 26) . ((-) 97)

keywordOffset :: Keyword -> PlaintextIndex -> Offset
keywordOffset (Key keyword) = toOffset . (!!) (downcase keyword) . (flip mod $ length keyword)

toOffset :: Char -> Offset
toOffset = (flip mod 97) . ord

downcase :: String -> String
downcase = map toLower

withIndex :: String -> [(Int, Char)]
withIndex string = init $ scanr itemAndIndex (strLength - 2, ' ') string where
  strLength = length string
  itemAndIndex ' ' prev  = (fst prev, ' ')
  itemAndIndex char prev = (fst prev - 1, char)

-- starts at 97, goes to 122, diff 25

-- reduce each ascii by 97
-- add them
-- mod 25
-- inflate by 97


-- make an ASCII product type, define functor on that shit

-- full ascii range 32 to 126, diff 94
