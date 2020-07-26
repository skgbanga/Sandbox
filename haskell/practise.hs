import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.Regex.Posix

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
   | (x == y)  = True
   | otherwise = elem' x ys

sum' :: (Integral a) => a
sum' = sum (map (^2) (filter odd [1..10]))

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
   | even n = n : collatz (n `quot` 2)
   | odd n  = n : collatz ( 3 * n + 1 )

chain :: (Integral a) => a
chain = sum [1 | x <- [1..100], (length (collatz x)) > 15]

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- checks whether a string (list of chars) is a substr of another string(list of chars)
-- e.g. whether "hulk" is part of "thorandhulk"
--
-- List.tails "Hulk" -> ["Hulk", "ulk", "lk", "k", ""]
-- So we define a function which takes a word, and compares it against our give needle (only the length)
-- and then we run this function on all the tails of the haystack
-- And then we apply List.any (std::any_of in C++) to check if we have a match
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = List.any match tails
   where tails = List.tails haystack
         match word = (take (length needle) word) == needle

-- a recursive implementation of search
search' :: [Char] -> [Char] -> Bool
search' needle [] = False
search' needle haystack
   | match haystack == True = True
   | otherwise = search' needle (tail haystack)
      where match word = (take (length needle) haystack) == needle

-- School locker room

data LockerState = Taken | Free deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lookup' :: Int -> LockerMap -> Either String Code
lookup' num_ map_ =
   case (Map.lookup num_ map_) of
      Nothing -> Left $ "LockerNumber " ++ show num_ ++ " doesn't exist!"
      Just (state, code)
         | state == Taken  -> Left $ "Ha"
         | otherwise       -> Right $ "Haha"


-- Make Map k part of the functor
-- keys :: (Ord k) => Map.Map k a -> [k]
-- keys fromList [] = []
-- keys fromList $ [(k, v)] : rest = k : (keys $ fromList rest)

deleteByIndex :: (Eq a) => Int -> [a] -> [a]
deleteByIndex n xs
   | n >= length xs = [] -- ideally we should raise an exception
   | otherwise = List.delete (xs !! n) xs

-- getString :: String -> [String]
-- getString x = snd getStringImpl x ""

-- 1
lookupImpl :: String -> Map.Map String String -> Maybe String
lookupImpl x m = Map.lookup ((last . words) x) m

-- 2
optToVal :: Maybe String -> String
optToVal Nothing = ""
optToVal (Just s) = s

-- 3
initStr :: String -> String
initStr = List.intercalate " " . init . words

-- 4
getStringImpl :: String -> String -> (String, String)
getStringImpl [] y = ([], y)
getStringImpl (x:xs) y
   | x == '{' = getStringImpl xs y
   | x == '}' = getStringImpl xs ((initStr y) ++ " " ++ (optToVal (lookupImpl y map_)))
   | otherwise = getStringImpl xs (y ++ [x])
      where map_ = Map.fromList [("hobbit1", "frodo"), ("hobbit2", "sam"), ("relation", "friends")]
