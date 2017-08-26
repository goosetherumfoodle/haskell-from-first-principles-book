module StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)
import Debug.Trace (trace)
import Data.Monoid ((<>))

-- from 1159

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if (trace ("i is " <> show i) i) `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)

-- Try modifying this one so that both exceptions are handled and the loop
-- never terminates.

main' :: IO ()
main' = forever $ do
  randomException `catch` handler
  -- microseconds
  threadDelay (1 * 1000000) where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ "got exception: " <> show e


-- pg 1160:
-- The conventional way to throw an exception is to use
-- throwIO, which has IO in its result. This is the same thing as throw, but
-- throwIO embeds the exception in IO. You always handle exceptions
-- in IO 3 . Handling exceptions must be done in IO even if they were
-- thrown without an IO type. You almost never want throw as it throws
-- exceptions without any warning in the type, even IO.
