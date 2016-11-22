module Ch15 where

import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Optional a = Nada | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (mappend a b)
  mappend Nada Nada = Nada
  mappend Nada x = x
  mappend x Nada = x

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into the car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ",
           adv, " as he jumped into the car ",
           noun, " and drove off with his ",
           adj, " wife."]

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

data Bull = Fools | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend x mempty = x
  mappend mempty x = x

type BullMapped = Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: BullMapped)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)

newtype First' a = First' { getFirst' :: Optional a }
                 deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend x mempty = x
  mappend x mempty = x

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend = First' String
                 -> First' String
                 -> First' String

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRigthIdentity :: FstId)
