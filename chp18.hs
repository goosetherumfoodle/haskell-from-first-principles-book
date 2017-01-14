import Data.Monoid ((<>))
import Control.Monad (join, (>=>))
import Control.Applicative ((*>), liftA2)

mmap f xs = xs >>= return . f

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

-- `do` sytax sugar.
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

-- `do` syntax variable binding
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

-- page 736

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)


twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
           ++ name ++ " who is "
           ++ age ++ " years old.")

twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls:" >>
  getLine >>=
  \age ->
  putStrLn ("y helo thar: "
           ++ name ++ " who is "
           ++ age ++ " years old.")

-- pg 738

twiceWhenEven :: Integral a => [a] -> [a]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: Integral a => [a] -> [a]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

-- pg 738
-- maybe monad

data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: [Char] -> Maybe [Char]
noEmpty "" = Nothing
noEmpty str = Just str


noNegative :: (Ord a, Num a) => a -> Maybe a
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: [Char] -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just name ->
      case noNegative age' of
        Nothing -> Nothing
        Just age ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weight ->
              weightCheck (Cow name age weight)

-- cleaned up with monad

mkSphericalCow' :: [Char] -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  name <- noEmpty name'
  age <- noNegative age'
  weight <- noNegative weight'
  weightCheck (Cow name age weight)

-- with (>>=)

mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \name ->
    noNegative age' >>=
    \age ->
      noNegative weight' >>=
      \weight ->
        weightCheck (Cow name age weight)

-- page 742

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integral a => a -> Maybe a
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Show a => a -> Maybe [Char]
h i = Just ("1091" ++ show i)

doSomething' :: Integer -> Maybe (Integer, Integer, [Char])
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- pg 749
-- either monad

type Founded = Int
type Coders = Int
data SoftwareShop = Shop {
                         founded :: Founded
                       , programmers :: Coders
                       } deriving (Eq, Show)

data FoundedError = NegativeYears Founded
                  | TooManyYears Founded
                  | NegativeCoders Coders
                  | TooManyCoders Coders
                  | TooManyCodersForYears Founded Coders
                  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Int
validateFounded n | n < 0 = Left $ NegativeYears n
                  | n > 500 = Left $ TooManyYears n
                  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Int
validateCoders n | n < 0 = Left $ NegativeCoders n
                 | n > 5000 = Left $ TooManyCoders n
                 | otherwise = Right n

mkSoftware :: Founded -> Coders -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers

-- ex pg 751 Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure a = Second a

  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second a) (Second b) = Second $ a b

instance Monad (Sum a) where
  return = pure

  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a

-- application and composition pg 759

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join $ f <$> (g a)

-- we can replace `join` and `fmap` with `>>=`

mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f

-- Kleisli composition is for compostion in monads

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) ::         (a -> b) -> (b -> c) -> a -> c

-- pg 762

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"

-- chapter excercises pg 763

-- 1

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= _ = NopeDotJpg

-- 2

data PhbtEither b a = Left' a | Right' b

instance Functor (PhbtEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' $ f a

instance Applicative (PhbtEither b) where
  pure a = Left' a

  (Right' a) <*> _ = Right' a
  (Left' _) <*> (Right' a) = Right' a
  (Left' a) <*> (Left' b) = Left' $ a b

instance Monad (PhbtEither b) where
  (Right' a) >>= _ = Right' a
  (Left' a) >>= f = f a

  (Right' a) >> _ = Right' a
  (Left' _) >> a = a

-- 3

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a

  Identity a <*> Identity b = Identity $ a b

instance Monad Identity where
  (Identity a) >>= f = f a

-- 4

data List a = Nil | Cons a (List a) deriving (Show)

instance Monoid (List a) where
  mempty = Nil

  mappend Nil a = a
  mappend a Nil = a
  mappend (Cons car cdr) list2 = Cons car (cdr `mappend` list2)

instance Functor List where
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

instance Applicative List where
  pure a = Cons a Nil

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons car cdr) <*> list2 = (car <$> list2) <> (cdr <*> list2)

instance Monad List where
  return = pure

  Nil >>= _ = Nil
  (Cons car cdr) >>= f = (f car) <> (cdr >>= f)

-- pg 764 write the following functions using...

-- 1

j :: Monad m => m (m a) -> m a
j = join

-- 2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

-- 4

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return $ []
meh (x:xs) f = liftA2 (<>) (makeList <$> f x) (meh xs f) where
  makeList a = [a]

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
