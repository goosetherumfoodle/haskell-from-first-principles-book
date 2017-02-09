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

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' = liftA2 Dog dogName address

-- Ex Reading Comprehension pg 848

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 a b c = a <$> b <*> c

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure a = Reader $ \ x -> a

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ (\r -> (rab r)) <*> ra
