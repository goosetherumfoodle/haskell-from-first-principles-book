printInc n = let plusTwo = n + 2
             in print plusTwo

printInc' n =
  (\plusTwo -> print plusTwo) (n + 2)

ex1 = let x = 3; y = 1000 in x * 3 + y

ex1' = x * 3 + y where
  x = 3
  y = 1000

waxOn = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8

hello :: String
hello = "hello"

world :: String
world = "world!"

main = do
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]

area d = pi * (r * r)
  where r = d / 2
