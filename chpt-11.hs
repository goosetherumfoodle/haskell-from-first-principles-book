import Data.Char
import Data.List

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultR'Us
  | TakeYourChanceUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

data Size = Big
          | Medium
          | Small
          deriving (Eq, Show)

data Price = Price Integer
           deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir Small

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> Bool
areCars = (foldr (&&) True) . (map isCar)

getManu :: Vehicle -> Manufacturer
getManu (Car manufactorer _) = manufactorer

class TooMany a where
  tooMany :: a -> Bool

newtype NamedThings = NamedThings (Int, String) deriving Show

newtype TwoThings = TwoThings (Int, Int) deriving Show

instance TooMany NamedThings where
  tooMany (NamedThings (num, _)) = num > 42

instance TooMany TwoThings where
  tooMany (TwoThings (first, second)) = (first + second) > 42

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data OperatingSystem = GnuLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = foldr buildProgrammers [] allOperatingSystems where
  buildProgrammers os accum = map (Programmer os) allLanguages ++ accum

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                         deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f base tree = foldr f base (inorder tree)

type Key = String
type Plaintext = String
type Ciphertext = String
type PlainChar = Char
type CipherChar = Char
type PlaintextIndex = Int
type Offset = Int

vigenereEncipher :: Key -> Plaintext -> Ciphertext
vigenereEncipher keyword = map (rotate keyword) . withIndex . downcase where

vigenereDecipher :: Key -> Ciphertext -> Plaintext
vigenereDecipher keyword = map (reverseRotate keyword) . withIndex . downcase where

  -- this name sucks
rotate :: Key -> (PlaintextIndex, PlainChar) -> CipherChar
rotate keyword (_, ' ') = ' '
rotate keyword (pi, pchar) = intoAToZASCII $ (ord pchar) + (keywordOffset keyword pi)

  -- ditto
reverseRotate :: Key -> (PlaintextIndex, PlainChar) -> CipherChar
reverseRotate keyword (_, ' ') = ' '
reverseRotate keyword (pi, pchar) = intoAToZASCII $  (ord pchar) - (keywordOffset keyword pi)

intoAToZASCII :: Int -> Char
intoAToZASCII = chr . (+ 97) . (flip mod 26) . (flip (-) 97)

keywordOffset :: Key -> PlaintextIndex -> Offset
keywordOffset keyword = toOffset . (!!) (downcase keyword) . (flip mod $ length keyword)

toOffset :: Char -> Offset
toOffset = (flip mod 97) . ord

downcase :: String -> String
downcase = map toLower

withIndex :: String -> [(Int, Char)]
withIndex string = init (scanr itemAndIndex (strLength - 2, ' ') string) where
  strLength = length string
  itemAndIndex ' ' prev = (((fst prev)), ' ')
  itemAndIndex x prev = (((fst prev) - 1), x)

    -- should have used as-patterns, but didn't seem to need it
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf sub super = all (flip elem super) sub

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map capitalizeSecond) . (map doubleUp) . words where
  doubleUp word = (word, word)
  capitalizeSecond (first, second) = (first, (capitalize second))
  capitalize (x:xs) = (toUpper x) : xs

data DaPhone' = DaPhone' (PhoneRow, PhoneRow, PhoneRow, PhoneRow) deriving (Show)

data DaPhone = DaPhone [Button] deriving (Show)

data PhoneRow = PhoneRow (Button, Button, Button) deriving (Show)

data Button' = AlphaNumButton' (Integer, [Char])
             | NumButton' Integer
             | SymbolButton' (Char, [Char])
             deriving (Show)

data Symbol = Integer | Char deriving (Show)

data Button = AlphaNumButton Integer [Char]
             | NumButton Integer
             | SymbolButton Char [Char]
            deriving (Show)


-- basicPhone' = DaPhone' ((PhoneRow ((NumButton 1)
--                                  ,(AlphaNumButton (2, "ABC"))
--                                  ,(AlphaNumButton (3, "DEF"))))

--                      ,(PhoneRow ((AlphaNumButton (4, "GHI")
--                                ,(AlphaNumButton (5, "JKL"))
--                                ,(AlphaNumButton (6, "NMO")))))

--                      ,(PhoneRow ((AlphaNumButton (4, "GHI"))
--                                ,(AlphaNumButton (5, "JKL"))
--                                ,(AlphaNumButton (6, "NMO"))))

--                      ,(PhoneRow ((SymbolButton ('*', "^"))
--                                ,(AlphaNumButton (0, "+_"))
--                                ,(SymbolButton ('#', ".,")))))

basicPhone = DaPhone [(NumButton 1)
                   ,(AlphaNumButton 2 "ABC")
                   ,(AlphaNumButton 3 "DEF")
                   ,(AlphaNumButton 4 "GHI")
                   ,(AlphaNumButton 5 "JKL")
                   ,(AlphaNumButton 6 "NMO")
                   ,(AlphaNumButton 4 "GHI")
                   ,(AlphaNumButton 5 "JKL")
                   ,(AlphaNumButton 6 "NMO")
                   ,(SymbolButton '*' "^")
                   ,(AlphaNumButton 0 "+_")
                   ,(SymbolButton '#' ".,")
                   ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

-- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
-- reverseTaps (DaPhone row1 row2 row3 row4) = map fetchChar [row1, row2, row3, row4]

reverseTaps :: DaPhone -> Char -> Integer-- [(Digit, Presses)]
reverseTaps (DaPhone buttonList) char = maybeInc (getPresses char (find (hasChar char) buttonList)) where
  maybeInc :: Maybe Int -> Integer
  maybeInc (Just x) = (toInteger x) + 1
  maybeInc Nothing  = 0

getPresses :: Char -> Maybe Button -> Maybe Int
getPresses char (Just (AlphaNumButton _ chars)) = elemIndex char chars
getPresses char (Just (SymbolButton _ chars)) = elemIndex char chars
getPresses char (Just (NumButton _)) = Nothing
getPresses char Nothing = Nothing

hasChar :: Char -> Button -> Bool
hasChar _ (NumButton _) = False
hasChar char (AlphaNumButton bSym chars) | elem char chars = True
                                         | otherwise = False
hasChar char (SymbolButton symbol chars) = elem char chars

-- fetchCharTaps :: Button -> Char -> Maybe [(Digit, Presses)]
-- fetchCharTaps (AlphaNumButton (digit, string)) char
--   | elem char string = Just [((intToDigit digit), (elemIndex char string))]

--     -- todo: finish phone exercise

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit num) = num
eval (Add a b) = (eval a) + (eval b)

printExpr :: Expr -> String
printExpr (Lit num) = show num
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)
