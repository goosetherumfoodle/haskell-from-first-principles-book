{-# LANGUAGE InstanceSigs #-}
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


import Data.Char (ord, chr)
import System.Environment (getArgs)
import System.IO (stdin, stdout, hPutStr, hGetLine, hGetContents)
import Control.Applicative (liftA2)

newtype Key a = Key a deriving (Show, Functor)
newtype ASCIInum a = ASCIInum a deriving Show

data EncryptionFlag = EncryptFlag | DecryptFlag

type Keyword = Key String
type Plaintext = String
type Ciphertext = String
type KeyChar = Char

main :: IO ()
main = do flag <- getFlag
          case flag of
            EncryptFlag -> mainEncrypt
            DecryptFlag -> mainDecrypt

toASCIInum :: Char -> Maybe (ASCIInum Int)
toASCIInum a | (32 <= ord a) && ord a <= 126 = Just $ ASCIInum $ ord a
             | otherwise = Nothing

toASCIInum' :: Char -> ASCIInum Int
toASCIInum' = ASCIInum . ord

fromASCIInum :: ASCIInum Int -> Char
fromASCIInum (ASCIInum a) = chr a

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
encipherPair = map (fromASCIInum . combine) where
  combine = liftA2 (+) (toASCIInum' . fst) (toASCIInum' . snd)

decipherPair :: [(KeyChar, Char)] -> Plaintext
decipherPair = map (fromASCIInum . combine) where
  combine  = liftA2 (-) (toASCIInum' . snd) (toASCIInum' . fst)

instance (Integral a, Eq a) => Num (ASCIInum a) where
  (ASCIInum a) + (ASCIInum b) = ASCIInum $ (+ 31) $ (flip mod 95) $ (a - 31) + (b - 31)
  (ASCIInum a) - (ASCIInum b) = ASCIInum $ (+ 31) $ (flip mod 95) $ (a - 31) - (b - 31)
  (ASCIInum a) * (ASCIInum b) = ASCIInum $ (+ 31) $ (flip mod 95) $ (a - 31) * (b - 31)
  abs (ASCIInum a) = ASCIInum $ (+ 31) $ (flip mod 95) $ abs $ (a - 31)
  fromInteger = ASCIInum . fromInteger . (+ 31) . (flip mod 95)
  signum (ASCIInum a) | a > 0 = 1
                      | a == 0 = 0
                      | otherwise = (-1)

-- look at Data.Modular

-- instance Functor ASCIInum where
--   fmap :: Integral a => (a -> b) -> f a -> f b
--   fmap f (ASCIInum a) = ASCIInum $ f (a + 10)
