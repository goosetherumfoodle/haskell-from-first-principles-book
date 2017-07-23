{-# LANGUAGE Strict #-}

module StrictList where

data List a = Nil | Cons a (List a) deriving (Show)

take' :: (Num t, Ord t) => t -> List a -> List a
take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) $ map' f xs

repeat' :: t -> List t
repeat' x = xs where xs = Cons x xs
