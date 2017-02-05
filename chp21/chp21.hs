import Data.Monoid
import Control.Applicative (liftA2)
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



-- (((+) .) . (+)) :: Num a => a -> a -> a -> a

-- pg 822 refactoring:

-- pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn query = do
--   a <- fetchFn query
--   case sequence (map decodeFn a) of
--     (Left err) -> return $ Left $ err
--     (Right res) -> do
--       a <- makeIoOnlyObj res
--       return $ Right a

-- gets refactored into:
-- pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn query = do
--   a <- fetchFn query
--   traverse makeIoOnlyObj (traverse decodeFn a)

-- point-free:
-- pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

-- pg 824

-- runIdentity :: Identity a -> a
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

edgelordMap :: Traversable t => (a -> b) -> t a -> t b
edgelordMap f t = runIdentity $ traverse (Identity . f) t

-- chpt ex pg 829

-- Identity

newtype Identity' a = Identity' a deriving (Show, Eq)

instance Foldable Identity' where
  foldMap f (Identity' a) = f a

instance Functor Identity' where
  fmap f (Identity' a) = Identity' $ f a

instance Eq a => EqProp (Identity' a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    arbitrary >>= \a -> return $ Identity' a

instance Traversable Identity' where
  sequenceA (Identity' fa) = Identity' <$> fa
  traverse f (Identity' a) = Identity' <$> f a

-- Constant

-- newtype Constant a b = Constant { getConstant :: a } deriving (Show, Eq)

-- instance Functor (Constant a) where
--   fmap _ (Constant a) = Constant a

-- instance Foldable (Constant a) where
--   foldMap f (Constant a) = mempty

-- -- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- -- traverse  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- instance Traversable (Constant a) where
--   sequenceA (Constant a) = Constant mempty
--   -- traverse f (Constant a) = Constant a mempty

-- instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
--   arbitrary = arbitrary >>= \a -> return $ Constant a

-- Maybe

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep a) = Yep <$> a

  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(20, return $ Yep a), (1, return Nada)]

-- List

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (f <$> b)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a b) = (f a) <> (foldMap f b)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons a b) = liftA2 Cons a (sequenceA b)

  traverse _ Nil = pure Nil
  traverse f (Cons a b) = liftA2 Cons (f a) (traverse f b)

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(20, return $ Cons a b), (1, return $ Cons a Nil)]

-- Three

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  sequenceA (Three a b c) = (Three a b) <$> c

  traverse f (Three a b c) = (Three a b) <$> f c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- Three'

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

instance Traversable (Three' a) where
  sequenceA (Three' a b c) = liftA2 (Three' a) b c

  traverse f (Three' a b c) = liftA2 (Three' a) (f b) (f c)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

-- S

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) $ f a

instance Foldable (S n) where
  foldMap f (S na a) = f a

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

instance Traversable n => Traversable (S n) where
  --         t   fa   f ta
  sequenceA (S n a) = (S (sequenceA n)) <$> a


instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = do
    n <- arbitrary
    a <- arbitrary
    return $ S n a

main = do

  -- Identity'
  quickBatch $ functor (undefined :: Identity' (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Identity' (Int, Int, [Int]))

  -- -- Constant
  -- quickBatch $ functor (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int]))

  -- Optional
  quickBatch $ functor (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))

  -- List
  quickBatch $ functor (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))

  -- Three
  quickBatch $ functor (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]))

    -- Three
  quickBatch $ functor (undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int]))

    -- S
  quickBatch $ functor (undefined :: S Identity' (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: S Identity' (Int, Int, [Int]))
