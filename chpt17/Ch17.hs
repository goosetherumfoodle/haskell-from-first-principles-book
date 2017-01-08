module Ch17 where

import Data.List (elemIndex)
import Control.Applicative (liftA2)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- ex Lookups - 684

-- 1

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- todo: ?
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4

xs :: [Integer]
xs = [1, 2, 3]
ys :: [Integer]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

-- Ex Identity Instance pg. 686

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity a) (Identity b) = Identity $ a b

-- Ex Constant Instance pg 688

newtype Constant a b = Constant { getConstant :: a } deriving Show

-- todo

-- example pg 689 Maybe Person

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

-- without Applicative:
mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of
                 Nothing -> Nothing
                 Just n' ->
                   case mkAddress a of
                     Nothing -> Nothing
                     Just a' -> Just $ Person n' a'

-- with Applicative:

mkPerson' :: String -> String -> Maybe Person
mkPerson' name addr = Person <$> mkName name <*> mkAddress addr

-- or:

mkPerson'' :: String -> String -> Maybe Person
mkPerson'' name addr = liftA2 Person (mkName name) (mkAddress addr)

-- ex: Fixer Upper pg 701

-- 1

answerOne :: Maybe String
answerOne = const <$> Just "Hello" <*> pure "world"

-- 2

answerTwo :: Maybe (Integer, Integer, [Char], [Integer])
answerTwo = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

instance Applicative List where
  pure car = Cons car Nil

  (<*>) (Cons car cdr) (Cons car' cdr') = Cons (car car') $ cdr <*> cdr'
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
