import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate = foldl consUTC [] where
  consUTC utcList item2 = utcList ++ (getUTC item2)
  getUTC (DbDate utc) = [utc]
  getUTC _ = []

filterDbNumber = foldl consNumber [] where
  consNumber numList item = numList ++ (getInteger item)
  getInteger (DbNumber num) = [num]
  getInteger _ = []

mostRecent = foldl latestDate lowDate where
  lowDate = DbDate (UTCTime (fromGregorian 0 0 00) (secondsToDiffTime 34123))
  latestDate prevDate date = max prevDate date

sumDb = foldl sumVals 0 where
  sumVals prevVal val = prevVal + (getInteger val)
  getInteger (DbNumber num) = num
  getInteger _ = 0

fibs = 1 : scanl (+) 1 fibs
fibsN = (!!) fibs

fibs20 = take 20 fibs
fibs100 = filter ((>) 1000) fibs

fact x = scanl (*) 1 [2..x]

stopPairs stops = concat $ map (\x -> map (\y -> [x, y]) stops) stops

stopvows stops vowels = concat $ map (\ v -> map (\ stop -> (head stop, v, head $ tail stop)) (stopPairs stops)) vowels

-- stopvowsap = filter pStart . (concat . (uncurry stopvows)) where
--   pStart ('p', _, _) = True
--   pStart _           = False

stopvowsap stops vowels = filter pStart (stopvows stops vowels) where
  pStart ('p', _, _) = True
  pStart _           = False

stopvowsap' = (filter pStart .) . stopvows where
  pStart ('p', _, _) = True
  pStart _           = False

nounverber = stopvows

myAnd []     = True
myAnd (x:xs) =
  if x == False
  then False
  else myAnd xs

myAnd' = foldr (&&) True

myOr = foldr (||) False

myAny = (flip foldr) False

myElem :: Eq a => a -> [a] -> Bool
myElem elem list = foldr (contains elem) False list where
  contains :: Eq a => a -> a -> Bool -> Bool
  contains elem current found = found || (elem == current)

-- fold(l/r) fun False [item1..item3]
-- (((False `fun` item1) `fun' item2) `fun` item3)
-- (item1 `fun` (item2 `fun` (item3 `fun` False)))

myReverse :: [a] -> [a]
myReverse = foldr (\ x y -> y ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap fun = foldr (\x out -> (fun x) : out ) []

myMapl :: (a -> b) -> [a] -> [b]
myMapl fun = foldl (\out x ->  out++[fun x]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr buildList [] where
  buildList elem out = if (pred elem)
                       then elem : out
                       else out

mySquish :: [[a]] -> [a]
mySquish = foldr (++) []

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap func = foldr (\elem out -> (func elem) ++ out) []

squishAgain :: [[a]] -> [a]
squishAgain = mySquishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmpr list = foldr doCompare (head list) list where
  doCompare item prevMax = if (cmpr item prevMax) == GT
                           then item
                           else prevMax

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmpr list = foldr doCompare (head list) list where
  doCompare item prevMin = if (cmpr item prevMin) == LT
                           then item
                           else prevMin

           -- 362

-- foldr const 0 [1..5]
