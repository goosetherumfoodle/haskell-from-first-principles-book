module Main where

import Control.Applicative (liftA2)
import Control.Monad (join)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- pg 976

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a

  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ liftA2 (<*>) mab ma

-- pg 979

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
                        v <- ma
                        case v of
                          Nothing  -> return Nothing
                          Just y -> runMaybeT $ f y


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

  (EitherT meea) >>= atemb = EitherT $ do
    x <- meea
    case x of
      Left e ->  return $ Left e
      Right a -> runEitherT $ atemb a


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


-- pg 982
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \r -> pure a

  (ReaderT rmf) <*> (ReaderT rmb) = ReaderT $ liftA2 (<*>) rmf rmb

instance Monad m => Monad (ReaderT r m) where
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= flip runReaderT r . f

-- pg 984

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- 1. You’ll have to do the Functor and Applicative instances first,
-- because there aren’t Functor and Applicative instances ready to
-- go for the type Monad m => s -> m (a, s)
-- a -> (b, s)

instance (Functor m) => Functor (StateT s m) where
  fmap f m = StateT $ (liftA2 (,) (f . fst) snd <$>) . runStateT m

-- 2. As with Functor, you can’t cheat and re-use an underlying Ap-
-- plicative instance, so you’ll have to do the work with the s -> m
-- (a, s) type yourself.

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT smf) <*> (StateT sma) = StateT $ \s -> (flip (,) s <$>)
                                                 $ (fst <$> smf s)
                                                 <*> (fst <$> sma s)

-- Also note that the constraint on m is not Applicative as you ex-
-- pect, but rather Monad. This is because you can’t express the
-- order-dependent computation you’d expect the StateT Applica-
-- tive to have without having a Monad for m. To learn more,
-- see this Stack Overflow question 1 about this issue. Also see this
-- Github issue 2 on the NICTA Course Github repository. Beware!
-- The NICTA course issue gives away the answer. In essence, the
-- issue is that without Monad, you’re just feeding the initial state
-- to each computation in StateT rather than threading it through
-- as you go. This is a general pattern contrasting Applicative and
-- Monad and is worth contemplating.

-- 3. The Monad instance should look fairly similar to the Monad
-- instance you wrote for ReaderT.

-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (StateT smas) >>= f = StateT $ \s -> smas s >>= flip runStateT s . f . fst

-- pg 990

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

-- pg 992
-- Turn readerUnwrap from the previous example back into embedded
-- through the use of the data constructors for each transformer.

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = undefined

main :: IO ()
main = putStrLn "hello world"
