import Data.Char

myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = (thisWord xs):myWords (nextWords xs) where
  thisWord = takeWhile (/= ' ')
  nextWords = dropWhile (/= ' ')

myLen [] = 0
myLen (x:xs) = 1 + myLen xs

  -- pg 354 1
mulThree = length $ filter mulOfThree [1..30] where
  mulOfThree = (== 0) . flip rem 3

    -- 3
myFilter = filter articles . words where
  articles word
    | word == "the" = False
    | word == "a"   = False
    | word == "an"  = False
    | otherwise     = True

    -- 1
myOr [] = False
myOr (x:xs) = x || myOr xs

  -- 2
myAny f [] = False
myAny f (x:xs) = f x || myOr xs

  -- 3
myElem elem []     = False
myElem elem (x:xs) = elem == x || myElem elem xs

  -- 4
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

  -- 5
squish [] = []
squish (x:xs) = x ++ squish xs

  -- 6
squishMap f [] = []
squishMap f (x:xs) = f x : squishMap f xs

  -- 7
squishAgain list = squishMap id list


mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]
