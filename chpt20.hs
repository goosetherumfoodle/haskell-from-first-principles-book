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

null :: (Foldable t) => t a -> Bool
null ta = foldr (\ a b -> )



toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

length' :: (Foldable t) => t a -> Int
length' = foldr (\ _ count -> count + 1) 0 where

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty
