import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum <$> foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct <$> foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap (Any . (== a)) ta

-- minimum' :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum'

-- 5. maximum :: (Foldable t, Ord a) => t a -> Maybe a

null' :: (Foldable t) => t a -> Bool
null' =  ((== 0) . length')

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

length' :: (Foldable t) => t a -> Int
length' = foldr (\ _ count -> count + 1) 0 where

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

-- chpt ex pg 809

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f (Constant a) = mempty
  foldr f init (Constant x) = init

-- 2

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
  foldr f init (Two a b) = f b init

-- 3

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
  foldr f init (Three a b c) = f c init

-- 4

data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c
  foldr f init (Three' _ b c) = f b $ f c init

-- 5

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d
  foldr f init (Four' a b c d) = f b $ f c $ f d init

-- Write a filter function for Foldable types using foldMap.

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred = foldMap (makeMonoid pred) where
  makeMonoid pred a | pred a = pure a
                    | otherwise = mempty
