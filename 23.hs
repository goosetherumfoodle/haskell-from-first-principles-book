-- needed for non-state solution
import System.Random
-- needed for state-based solution
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Monoid ((<>))

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


main = (rollsToGetTwenty . mkStdGen) <$> randomIO
