import Control.Applicative (liftA2)

newtype Reader r a = Reader { runReader :: r -> a }

-- ex Ask pg 845

ask :: Reader a a
ask = Reader id

-- pg 846

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Show, Eq)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Seseme Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDog' :: Person -> Dog
getDog' = \p -> Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Ex Reading Comprehension pg 848

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a b c = a <$> b <*> c

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure a = Reader $ \ x -> a

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra

-- pg 853

getDogM :: Person -> Dog
getDogM = do
  name <- dogName
  address <- address
  return $ Dog name address

getDogM' :: Person -> Dog
getDogM' = dogName >>= (\name -> address >>= (\addy -> return $ Dog name addy))


-- Hint: constrast the type with the Applicative instance and perform the most obvious change you can imagine to make it work
instance Monad (Reader r) where
  return = pure

-- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ (runReader . aRb . ra) <*> id
