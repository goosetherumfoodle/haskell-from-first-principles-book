{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch16 where

import Test.QuickCheck
import GHC.Arr

a :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Integer -> Integer
c = fmap (*2) (\x -> (x :: Integer) - 2)

d :: Integer -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

-- 16.10 Instances of Func

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 1

newtype Identity a = Identity a deriving Eq

instance Functor Identity where
  fmap func (Identity a) = Identity $ func a

-- 2

data Pair a = Pair a a deriving Eq

instance Functor Pair where
  fmap func (Pair a b) = Pair (func a) (func b)

-- 3

data Two a b = Two a b deriving Eq

instance Functor (Two a) where
  fmap func (Two a b) = Two a (func b)

-- 4

data Three a b c = Three a b c deriving Eq

instance Functor (Three a b) where
  fmap func (Three a b c) = Three a b $ func c

-- 5

data Three' a b = Three' a b b deriving Eq

instance Functor (Three' a) where
  fmap func (Three' a b c) = Three' a (func b) (func c)

-- 6

data Four a b c d = Four a b c d deriving Eq

instance Functor (Four a b c) where
  fmap func (Four a b c d) = Four a b c $ func d

-- 7

data Four' a b = Four' a a a b deriving Eq

instance Functor (Four' a) where
  fmap func (Four' a b c d) = Four' a b c $ func d

-- 8

-- this cannot be implemented because Trivial does not have the kind * -> *

-- Ex. Possibly

data Possibly a = Nope | Yep a deriving (Eq, Show)

instance Functor Possibly where
  fmap func (Yep a) = Yep $ func a
  fmap _ Nope = Nope

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return Nope), (5, fmap Yep arbitrary)]

-- Ex. pg. 651

-- 1

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap fun (Second a) = Second $ fun a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, fmap First arbitrary), (1, fmap Second arbitrary)]

-- pg. 654

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- pg 658

type Nat f g = forall a. f a -> g a

-- pg 661

data Tuple a b = Tuple a b deriving (Show, Eq)

instance Functor (Tuple a) where
  fmap f (Tuple a b) = Tuple a $ f b

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- chpt ex

-- 1
-- No, Bool would have to be of kind * -> *

-- 2

data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True' a) = True' $ f a

-- 3

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish $ f a

-- 4

newtype Mu f = InF { outF :: f (Mu f) }

-- Mu has kind (* -> *) -> *

-- 5

data D = D (Array Word Word) Int Int

-- D has kind *


-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1

data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' $ f a
  fmap _ (Second' b) = Second' b

-- 2

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1

data Quant a b = Finance | Desk a | Blor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Blor b) = Blor $ f b

-- 2

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
    -- todo
-- instance Functor (Flip K a) where
--fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap _ (Flip (K a)) = Flip (K a)
    -- not sure how to do this with a unary data constructor

-- 4

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

-- 5

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor g, Functor f) => Functor (Parappa f g) where
  fmap f (DaWrappa ab cd) = DaWrappa (fmap f ab) (fmap f cd)

-- 7

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

-- 8

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

-- 10

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11

data TalkToMe a = Halt | Print String a | Read (String -> a)

-- instance Functor TalkToMe where
--   fmap _ Halt = Halt
--   fmap f (Print String a) = Print String $ f a
--   fmap f (Read f') = Read $ f f'


main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])

  quickCheck $ \x -> functorIdentity $ Identity (x :: [Int])
  quickCheck $ \x -> functorCompose (+2) (*2) $ Identity (x :: Int)

  quickCheck $ \x -> functorIdentity $ Pair (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Pair (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ Two (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Two (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ Three (x :: Int) (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Three (x :: Int) (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ Three' (x :: Int) (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Three' (x :: Int) (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ Four (x :: Int) (x :: Int) (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Four (x :: Int) (x :: Int) (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ Four' (x :: Int) (x :: Int) (x :: Int) (x :: Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ Four' (x :: Int) (x :: Int) (x :: Int) (x :: Int)

  quickCheck $ \x -> functorIdentity $ (x :: Possibly Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ (x :: Possibly Int)

  quickCheck $ \x -> functorIdentity $ (x :: Sum Int Int)
  quickCheck $ \x -> functorCompose (+2) (*2) $ (x :: Sum Int Int)
