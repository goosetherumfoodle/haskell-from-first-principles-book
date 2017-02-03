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

-- null :: (Foldable t) => t a -> Bool
-- null ta = foldMap

foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []


-- 7. length :: (Foldable t) => t a -> Int
-- 8. Some say this is all Foldable amounts to.

-- 9. Hint: use foldMap.
-- -- | Combine the elements of a structure using a monoid.
-- fold :: (Foldable t, Monoid m) => t m -> m
-- 10. Define foldMap in terms of foldr.
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
