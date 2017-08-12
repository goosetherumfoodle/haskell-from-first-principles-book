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
