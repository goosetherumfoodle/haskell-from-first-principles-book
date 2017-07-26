{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

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
import Control.Monad (join)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)

newtype Key a = Key a deriving (Show, Functor)
newtype ASCIInum a = ASCIInum a deriving Show

data EncryptionFlag = EncryptFlag | DecryptFlag

type Keyword = Key String
type Plaintext = String
type Ciphertext = String
type KeyChar = Char

main :: IO ()
main = do flag <- runExceptT getFlag
          case flag of
            (Right EncryptFlag) -> join $ display <$> mainEncrypt
            (Right DecryptFlag) -> join $ display <$> mainDecrypt
            (Left problem) -> display problem

-- toASCIInum :: Char -> Maybe (ASCIInum Int)
-- toASCIInum a | (32 <= ord a) && ord a <= 126 = Just $ ASCIInum $ ord a
--              | otherwise = Nothing

toASCIInum' :: Char -> ASCIInum Int
toASCIInum' = ASCIInum . ord

fromASCIInum :: ASCIInum Int -> Char
fromASCIInum (ASCIInum a) = chr a

mainEncrypt :: IO String
mainEncrypt = getString <$> runExceptT results where
  results = do key <- foundKey
               plainText <- foundPlainText
               let cipherText = vigenereEncipher key plainText
               return cipherText
  getString (Left a) = a
  getString (Right a) = a

mainDecrypt :: IO String
mainDecrypt = getString <$> runExceptT results where
  results = do key <- foundKey
               ciphertext <- foundCiphertext
               let plainText = vigenereDecipher key ciphertext
               return plainText
  getString (Left a) = a
  getString (Right a) = a

getFlag :: ExceptT String IO EncryptionFlag
getFlag = do argList <- ExceptT $ Right <$> getArgs
             if length argList >= 1
               then ExceptT $ return $ buildFlag $ argList !! 0
               else ExceptT $ return $ Left "decrypt/encrypt flag not found in args"

buildFlag :: String -> Either String EncryptionFlag
buildFlag input | input == "-d" = Right DecryptFlag
                | input == "-D" = Right DecryptFlag
                | input == "-e" = Right EncryptFlag
                | input == "-E" = Right EncryptFlag
                | otherwise = Left $ "flag: " ++ input ++ " not recognized"

display :: String -> IO ()
display = hPutStr stdout . (++ "\n")

foundKey :: ExceptT String IO Keyword
foundKey = do argList <- ExceptT $ Right <$> getArgs
              if length argList >= 2
                then ExceptT $ return $ Right $ Key $ argList !! 1
                else ExceptT $ return $ Left "Key not found in arguments"

foundCiphertext :: ExceptT String IO String
foundCiphertext = ExceptT $ Right <$> hGetContents stdin

foundPlainText :: ExceptT String IO Plaintext
foundPlainText = ExceptT $ Right <$> hGetLine stdin

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
