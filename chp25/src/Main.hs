{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad(join)
import Data.Monoid((<>))

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- chpt 25 pg 956

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

-- difficulties with composing monads:
-- http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf

-- attempt:

-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return = pure

--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   (Compose fga) >>= aTofgb = aTofgb <$> (join fga)

-- pg 958
-- 25.6 Exercises: Compose Instances

-- Compose Foldable
-- Write the Foldable instance for Compose. The foldMap = undefined
-- bit is a hint to make it easier and look more like what you’ve seen
-- already.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap toM ta = foldr (\l r -> (toM l) <> r) mempty ta

  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  -- foldr aTobTob b ta = fmap (flip aTobTob b) ta

-- Compose Traversable
-- Write the Traversable instance for Compose.

instance (Traversable f, Traversable g, Applicative f) => Traversable (Compose f g) where
   traverse f comp = sequenceA $ f <$> comp


-- where does bifunctor get the expectation that p will be of kind * -> * -> *?

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1.
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

-- 2.
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

-- 3.
data Drei a b c = Drei a b c

instance Bifunctor (Drei a)  where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5.
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a

-- 6.
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7.
-- data Either a b = Left a | Right b
-- (already defined...)

instance Bifunctor Either where
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right b) = Right $ g b

-- Plain old Identity. 'a' can be something with
-- more structure, but it's not required and
-- Identity won't know anything about it.
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

-- The identity monad transformer, serving only to
-- to specify that additional structure should exist.
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

-- Functor

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

-- Applicative

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

-- Monad

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ Control.Monad.join $ fmap runIdentityT (fmap f ma)

main :: IO ()
main = do
  putStrLn "hello world"
