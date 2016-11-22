import Data.List

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

    -- pretend newtype is data for now
newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = True

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

    -- pg 223
    -- 1

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

    -- 2
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

    -- 3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

    -- 4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a _) (Pair a' _) = a == a'

    -- 5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'


    -- 6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

    -- 7
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye a) (Goodbye a') = a == a'
  (==) _ _ = False

    -- pg 231 1)
data Person = Person Bool
  deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

                     -- 2
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

  -- 4
type Subject = String
type Verb    = String
type Object  = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String deriving (Eq, Show)
data Yeah =
  Yeah Bool deriving (Eq, Show)
data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

  -- 8
myX = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX

  -- typekwondo
  -- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk func a b = func a == b

  -- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith func x y = func y

addFive x y = (if x > y then y else x) + 5
addFive' = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x
mflip' f x y = f y x
