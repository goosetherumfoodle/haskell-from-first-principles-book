{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ch17 where

import Data.Monoid ((<>))
import Data.List (elemIndex)
import Control.Applicative (liftA2, liftA3)
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

-- Ex List Applicative pg 712

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil

  mappend Nil list = list
  mappend list Nil = list
  mappend (Cons car cdr) list2 = (Cons car (mappend cdr list2))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

instance Applicative List where
  pure car = Cons car Nil

  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons car cdr) list2 = (car <$> list2) <> (cdr <*> list2)

-- pg 713

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f (Cons car cdr) = (f car) <> (flatMap f cdr)

pg713Answer :: List Integer
pg713Answer = flatMap (\x -> x `c` (9 `c` Nil)) xs where
  toMyList = foldr Cons Nil
  xs = toMyList [1, 2, 3]
  c = Cons

-- ex ZipList Applicative pg 714

take' :: Int -> List a -> List a
take' 0 _ = mempty
take' num (Cons car cdr) = Cons car $ take' (num - 1) cdr

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)

  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons car cdr)) (ZipList' (Cons car' cdr')) = ZipList' $ Cons (car car') (cdr <*> cdr')

-- Ex Variations on Either pg 719

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success $ f a
  fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
  pure a = Success a

  (<*>) (Success a) (Success b) = Success $ a b
  (<*>) (Failure err) (Failure err') = Failure $ err <> err'
  (<*>) _ (Failure err) = Failure err
  (<*>) (Failure err) _ = Failure err

-- chpt excercises pg 719

-- 1

pureList :: t -> [t]
pureList a = [a]

applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList _ [] = []
applyList (x:xs) (y:ys) = (x y) : (xs `applyList` ys) -- fix

-- 2

-- pureIO a = IO a

-- todo: not sure about instructions on 719

-- pg 720

-- 1
-- todo: quickcheck the following
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a

  (<*>) (Pair a a') (Pair b b') = Pair (a b) (a' b')

-- 2

data Two a b = Two a b deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance Monoid a => Applicative (Two a) where

  (<*>) (Two a b) (Two a' b') = Two (a <> a') (b b')

-- 3

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where

  (<*>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c c')

-- 4

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where

  (<*>) (Three' a b c) (Three' a' b' c') = Three' (a <> a') (b b') (c c')

-- 5

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where

  (<*>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d d')

-- 6

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

instance Monoid a => Applicative (Four' a) where

  (<*>) (Four' a b c d) (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (d d')

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c = liftA3 (,,) a b c
