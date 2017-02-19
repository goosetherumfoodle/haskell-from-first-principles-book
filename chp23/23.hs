-- needed for non-state solution
import System.Random
-- needed for state-based solution
import Control.Applicative (liftA3)
import Control.Monad
import Control.Monad.Trans.State
import Data.Monoid ((<>))
-- http://hackage.haskell.org/package/dlist
import qualified Data.DList as DL

-- begin non-state version
data Die = SideOne
         | SideTwo
         | SideThree
         | SideFour
         | SideFive
         | SideSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
               1 -> SideOne
               2 -> SideTwo
               3 -> SideThree
               4 -> SideFour
               5 -> SideFive
               6 -> SideSix
               -- don't use `error` in real code
               x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: Int -> (Die, Die, Die)
rollDieThreeTimes seed = do
  let s = mkStdGen seed
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)
-- end non-state version

-- begin state version
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- pg 870

-- repeats a single die value
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- replicateM :: Applicative m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- pg 871

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20 = count
    | otherwise =
    let (die, nextGen) = randomR (1, 6) gen
    in go (sum + die) (count + 1) nextGen

-- ex Roll Your Own pg 872

-- 1

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN target gen = go 0 0 gen where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= target = count
    | otherwise =
    let (die, nextGen) = randomR (1, 6) gen
    in go (sum + die) (count + 1) nextGen

-- 2

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged target g = go 0 0 g []  where
  go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
  go sum count gen dice
    | sum >= target = (count, dice)
    | otherwise =
    let (die, nextGen) = randomR (1, 6) gen
    in go (sum + die) (count + 1) nextGen (dice <> [intToDie die])

-- pg 873

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
-- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> (x s, s) where
    x = (f . fst) <$> g

instance Applicative (Moi s) where
-- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

-- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> ((getVal f s) (getVal g s), s) where
    getVal a = \s' -> (fst . a) s'

instance Monad (Moi s) where
  return = pure

-- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  moif@(Moi _) >>= g = join $ g <$> moif where

-- pg 874

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Monad m => Integer -> StateT [String] m () -- had to add `m ()`
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-- wtf!!! won't compile
-- fizzbuzzList' :: [Integer] -> DL.DList String
-- fizzbuzzList' list = execState (mapM_ addResult list) DL.empty

-- addResult' n = do
--   xs <- get
--   let result = fizzBuzz n
--   put (DL.snoc xs result)

-- ex 877 Fizzbuzz Differently
-- It’s an exercise! Rather than changing the underlying data structure,
-- fix our reversing fizzbuzz by changing the code in the following way:

-- fizzbuzzFromTo :: Integer -> Integer -> [String]
-- fizzbuzzFromTo = undefined

-- Continue to use consing in the construction of the result list, but
-- have it come out in the right order to begin with by enumerating the
-- sequence backwards. This sort of tactic is more commonly how you’ll
-- want to fix your code when you’re quashing unnecessary reversals.

-- 23 chpt exercises

-- 1 Construct a State where the state is also the value you return.

get' :: State s s
get' = state $ \s -> (s, s)

-- 2. Construct a State where the resulting state is the argument
-- provided and the value is defaulted to unit.

put' :: s -> State s ()
put' s = state $ \s -> ((), s)

-- 3. Run the State with s and get the state that results.

-- wtf is with this "State" data constructor?

-- exec :: State s a -> s -> s
-- exec (State sa) s = snd $ sa s

-- 4. Run the State with s and get the value that results.

-- again... what is this constructor?

-- eval :: State s a -> s -> a
-- eval (State sa) = ???

-- 5. Write a function which applies a function to create a new State.

myModify :: (s -> s) -> State s ()
myModify f = state $ \s -> ((), f s)

main = do
  (rollsToGetTwenty . mkStdGen) <$> randomIO
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
  -- mapM_ putStrLn $ fizzbuzzList' [1..100]
