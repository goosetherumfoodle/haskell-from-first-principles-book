-- the datatype Mood is represented by the values Blah or Woot
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood      -- pg 121
changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == backwardsX
  where backwardsX = reverse x

-- myAbs :: Integer -> Integer
-- myAbs x = if x > 0
--           then x
--           else 0 - x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (firstTup, secondTup)
  where firstTup = (snd x, snd y)
        secondTup = (fst x, fst y)
