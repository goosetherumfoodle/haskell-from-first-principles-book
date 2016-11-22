-- pg 257
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

g :: (a -> b) -> (a, c) -> (b, c)
g func (a, c) = ((func a), c)
