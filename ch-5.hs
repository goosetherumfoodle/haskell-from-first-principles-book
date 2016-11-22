nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer
                       -> Bool
                       -> Integer
typicalCurriedFunction i b =
  i + (nonsense b)

uncurriedFunction :: (Integer, Bool)
                  -> Integer
uncurriedFunction (i, b) =
  i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousManuallyNested :: Integer
                        -> Bool
                        -> Integer
anonymousManuallyNested =
  \i -> \b -> i + (nonsense b)

i :: a -> a
i x = x

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' a b = a

r :: [a] -> [a]
r list = reverse list

-- co :: (b -> c) -> (a -> b) -> (a -> c)
-- co f1 f2 = f1 $ f2

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x

-- pg 181

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x

      -- pg 182 ex 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

      -- pg 182 ex 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform t = (xz $ fst t, yz $ snd t)

          -- pg 182 ex 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 x = fst $ f2 $ f1 x
