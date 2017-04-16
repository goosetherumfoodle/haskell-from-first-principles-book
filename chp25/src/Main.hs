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

main :: IO ()
main = do
  putStrLn "hello world"
