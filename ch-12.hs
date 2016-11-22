notThe :: String -> Maybe String
notThe inputString
  | inputString == "the" = Nothing
  | inputString == "The" = Nothing
  | otherwise = Just inputString

replaceThe :: String -> String
replaceThe = unwords . (map replaceThe') . (map notThe) . words' where
  replaceThe' (Just word) = word
  replaceThe' Nothing = "a"

words' :: String -> [String]
words' sentence = words'' sentence []

words'' :: String -> [String] -> [String]
words'' [] wordList = wordList
words'' restSentence wordList = words'' remainingSentence (wordList ++ foundWord) where
  remainingSentence = (fst (consumeWord restSentence))
  foundWord = (snd (consumeWord restSentence))

consumeWord :: String -> (String, [String])
consumeWord sentence = consumeWord' sentence []

consumeWord' :: String -> String -> (String, [String])
consumeWord' [] word = ([], [word])
consumeWord' (' ' : restSentence) word = (restSentence, [word])
consumeWord' (char : restSentence) word = consumeWord' restSentence (word ++ [char])

countBeforeVowel :: String -> Integer
countBeforeVowel = fst . countBeforeVowel'

countBeforeVowel' = foldr doCount (0, False) . reverse . words'
doCount word (tally, prevThe)
    | word == "the" = (tally, True)
    | prevThe && (isVowel $ head word) = (tally + 1, False)
    | otherwise = (tally, False)
isVowel = (flip elem "aeiou")

countVowels :: String -> Int
countVowels = length . filter isVowel

newtype Word' = Word' String
                deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | tooManyVowels s = Nothing
  | otherwise = Just (Word' s) where
      tooManyVowels s = (vowelCount s) > (consonantCount s)
      vowelCount = fst . vowelsAndConsonants
      consonantCount = snd . vowelsAndConsonants
      vowelsAndConsonants = foldr collectVowsAndConsos (0, 0)
      collectVowsAndConsos char (vows, consos)
        | char == ' ' = (vows, consos)
        | isVowel char = (vows + 1, consos)
        | otherwise = (vows, consos + 1)

data Nat = Zero | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ num) = 1 + (natToInteger num)

integerToNat :: Integer -> Maybe Nat
integerToNat number
  | number < 0 = Nothing
  | number == 0 = Just Zero
  | otherwise = Just $ foldr buildNat Zero [1..number] where
      buildNat num nat = (Succ nat)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee dflt _ Nothing = dflt
mayybee _ func (Just val) = func val

fromMaybe :: a -> Maybe a -> a
fromMaybe dflt = mayybee dflt id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr getVal [] where
  getVal (Just x) valList = x : valList
  getVal Nothing valList = valList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr valsOrNothing (Just []) where
  valsOrNothing (Just x) (Just valList) = Just $ x : valList
  valsOrNothing _ _ = Nothing

lefts :: [Either a b] -> [a]
lefts = foldr getLefts [] where
  getLefts (Left x) leftList = x : leftList
  getLefts _ leftList = leftList

rights :: [Either a b] -> [b]
rights = foldr getRights [] where
  getRights (Right x) rightList = x : rightList
  getRights _ rightList = rightList

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr doPartition ([], []) where
  doPartition (Left x) (lefts, rights) = (x : lefts, rights)
  doPartition (Right x) (lefts, rights) = (lefts, x : rights)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe func (Right x) = Just $ func x
eitherMaybe _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' func _ (Left x) = func x
either' _ func (Right x) = func x

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' rightFunc = either' (\x -> Nothing) (Just . rightFunc)

myIterate :: (a -> a) -> a -> [a]
myIterate func val = myIterate' [] func val where
  myIterate' list func val = val : myIterate' list func (func val)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr func baseVal = myUnfoldr' (func baseVal) where
  myUnfoldr' Nothing = []
  myUnfoldr' (Just (lastVal, newVal)) = lastVal : (myUnfoldr' (func newVal))

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> (Just (a, (f a)))) x

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree func base = unfold' (func base) where
  unfold' Nothing = Leaf
  unfold' (Just (a, b, c)) = Node (unfold' (func a)) b (unfold' (func c))

treeBuild :: Integer -> BinaryTree Integer
treeBuild max = unfoldTree buildNode 0 where
  buildNode base | base == max = Nothing
                 | otherwise = Just (base + 1, base, base + 1)

-- makeTreeBuild :: a -> (a -> (a, a, a)) -> Integer -> BinaryTree Integer
-- makeTreeBuild init func = unfoldTree buildNode init where
--   buildNode :: a -> (a -> (a, a, a)) -> Maybe (a, a, a)
--   buildNode base | base == max = Nothing
--                  | otherwise = Just (func base)
