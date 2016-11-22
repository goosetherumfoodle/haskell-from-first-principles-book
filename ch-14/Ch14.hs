module Ch14 where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck


trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  frequency [(1, return Nothing)
           , (8, return (Just a))]

half x = x / 2

halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x

square x = x * x

  -- describe "square" $ do
  --   it "is reversible with sqrt" $ do
  --     property $ \x -> x == (square . sqrt) (x :: Double)


  -- todo: test ($) and 9, 10, 11, (... and the rest)

twice f = f . f
fourTimes = twice . twice

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "is associative" $ do
      property $ \x y z-> plusAssociative (x :: Int) (y :: Int) (z :: Int)
    it "is commutative" $ do
      property $ \x y -> plusCommutative (x :: Int) (y :: Int)
  describe "multiplication" $ do
    it "is associative" $ do
      property $ \x y z-> multAssociative (x :: Int) (y :: Int) (z :: Int)
    it "is commutative" $ do
      property $ \x y -> multCommutative (x :: Int) (y :: Int)
  describe "half" $ do
    it "is reversible" $ do
      property $ \x -> x == halfIdentity (x :: Double)
  describe "sort" $ do
    it "sorts lists" $ do
      property $ \x -> listOrdered $ sort (x :: [Int])
  describe "reverse" $ do
    it "is reversable" $ do
      property $ \list -> ((reverse . reverse) list) == (id list :: [Char])
