module Main where

import Control.Applicative (liftA2)
import Control.Monad (join)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- Exercises: EitherT
-- 1. Write the Functor instance for EitherT:

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT a) = EitherT $ (fmap . fmap) f a

-- -- 2. Write the Applicative instance for EitherT:

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT a) <*> (EitherT b) = EitherT $ liftA2 (<*>) a b

-- -- 3. Write the Monad instance for EitherT:

instance Monad m => Monad (EitherT e m) where
  return = pure

  v >>= f = join $ f <$> v

-- -- 4. Write the swapEitherT helper function for EitherT.

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

-- transformer version of swapEither.

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . (swapEither <$>) . runEitherT

-- -- Hint: write swapEither first, then swapEitherT in terms of the for-
-- -- mer.

-- -- 5. Write the transformer variant of the either catamorphism.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT a) = join $ step <$> a where
  step (Left a) = f a
  step (Right b) = g b

main :: IO ()
main = do
  putStrLn "hello world"
