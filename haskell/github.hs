-- https://github.com/noelmarkham/learn-you-a-haskell-exercises

import DistanceConversions
import qualified Data.List as List
import qualified Data.Set as Set
import System.Environment
import System.Random
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans.Writer

------ 02-starting out
-- penultimate
penultimate xs = last (init xs)

-- findK
findK k l = l !! k

-- isPalindrome
isPalindrome xs = xs == (reverse xs)

-- duplicate
duplicate xs = if null xs then xs else [(head xs), (head xs)] ++ duplicate(tail xs)
duplicate2 xs = concat [[x, x] | x <- xs]

-- ziplike
ziplike xs ys = if null xs || null ys then [] else [(head xs, head ys)] ++ ziplike (tail xs) (tail ys)

-- splitAtIndex
splitAtIndex k xs = (take k xs, drop k xs)

-- dropK
dropK k xs = concat [(take k xs), (drop (k + 1) xs)]

-- slice
slice i k xs = take (k - i) (drop i xs)

-- insert element
insertElem x k xs = take k xs ++ [x] ++ drop k xs

-- rotate
rotate n xs = take (length xs) (drop n (cycle xs))

------ 03 types and classes
data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

-- firstColor
firstColor = minBound :: Colour

-- list colours in reverse order
reverseColourOrder = reverse [(minBound :: Colour) .. (maxBound :: Colour)]

-- paintMix
-- 1 added to favour the higher mixture
paintMix :: Colour -> Colour -> Colour
paintMix c1 c2 = toEnum (( fromEnum c1 + fromEnum c2 + 1) `quot` 2)

-- englishDigit
englishDigit :: Int -> String
englishDigit x
   | x <= 9 && x >= 0 = show x
   | otherwise        = "unknown"

-- divTuple
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x / y

-- three zero list
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:_) = True
threeZeroList ys = False

-- pow
power :: (Num a, Integral b) => a -> b -> a
power _ 0 = 1
power x y = x * (power x (y - 1))

-- fib
fib :: (Num a, Eq a) => a -> [a]
fib 0 = [0]
fib 1 = [1, 0]
fib n = (x + y):prev
   where prev@(x:y:_) = fib (n - 1)

-- stepReverseSign
stepReverseSign :: (Num a, Ord a) => a -> a -> a
stepReverseSign x y
   | x > 0     = -1 * (x + y)
   | otherwise = -1 * x + y

-- CalcPi
leibniz :: (Integral a, Fractional b) => a -> b
leibniz n = (4 * num) / den
   where num = power (-1) n
         den = fromIntegral (2 * n + 1)

leibniz_sum :: (Integral a, Fractional b) => a -> b
leibniz_sum 0 = leibniz 0
leibniz_sum n = leibniz n + leibniz_sum (n - 1)

piCalc' :: (Fractional a, Integral b, Ord a) => a -> a -> b -> (a, b)
piCalc' v t n
   | abs x <= t = (v, n)
   | otherwise  = piCalc' (v + x) t (n + 1)
   where x = leibniz n

piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
piCalc a = piCalc' 0 a 0

sumInts :: Int -> Int -> Int
sumInts x y
   | x == y = y
   | otherwise = x + sumInts (x + 1) y

sq :: Int -> Int
sq n
   | n == 0 = 0
   | otherwise = sq (n - 1) + 2 * n - 1

sumSquares :: Int -> Int -> Int
sumSquares x y
   | x == y = (sq y)
   | otherwise = sq x + sumSquares (x + 1) y

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b = sum $ map intApplication [a..b]

hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum (^2)

hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (\x -> x)

higherOrderSequenceApplication :: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
higherOrderSequenceApplication f g a b = foldl1 f $ map g [a..b]

hoFactorial :: Int -> Int
hoFactorial = higherOrderSequenceApplication (*) id 1

areaConv :: (Float -> Float) -> Float -> Float
areaConv linear area = linear . linear $ area

sqInToSqCm :: Float -> Float
sqInToSqCm = areaConv inToCm

sqChainsToSqM :: Float -> Float
sqChainsToSqM = areaConv chToM

------ Type classes
data Suit  = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord)
data Digit = Ace | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Show, Ord)
data Card  = Card Digit Suit deriving (Eq)

instance Show Card where
   show (Card d s) = "The " ++ show d ++ " Of " ++ show s

betterCard :: Card -> Card -> Card
betterCard (Card d1 s1) (Card d2 s2)
   | s1 < s2 = (Card d2 s2)
   | (s1 == s2) && (d1 < d2) = (Card d2 s2)
   | otherwise = (Card d1 s1)

digitCard :: Card -> Digit
digitCard (Card d _) = d

suitCard :: Card -> Suit
suitCard (Card _ s) = s

data Coin = Head | Tail deriving (Eq)

class Hand a where
   play :: [a] -> Bool

instance Hand Card where
   play [] = False
   play (x:xs) =  aceFilter x || play xs
      where aceFilter c = ((digitCard c) == Ace)
   -- play xs = List.any (\x -> (digitCard x) == Ace) xs

instance Hand Coin where
   play xs = success `List.isInfixOf` xs
      where success = replicate 10 Head

-- inputs and ouputs

echo_sim :: [String] -> String
echo_sim [] = "\n"
echo_sim a@(x:xs)
   | x == "-n" = unwords xs
   | otherwise = (unwords a) ++ "\n"

main = do
   args <- getArgs
   putStr (echo_sim args)

lottery :: StdGen -> [Int]
lottery gen = take 6 $ List.nub $ randomRs (1, 49) gen

-- functors, applicative functors and monoids
data List a = Empty | Value a (List a) deriving (Show)

-- make the above list a functor
instance Functor List where
   fmap f Empty = Empty
   fmap f (Value a tail) = Value (f a) (fmap f tail)

-- a function to append one list to another (++)
combineLists :: List a -> List a -> List a
combineLists Empty x = x
combineLists (Value a tail) y = Value a (combineLists tail y)

-- make the above list a monoid
instance Monoid (List a) where
   mempty = Empty
   mappend = combineLists
   -- mconcat is implemented by default

-- make the above list an applicative
-- This is similar to ZipList as compared to applicative on functors
instance Applicative List where
   pure a = Value a Empty -- put it in default context so that [pure f <*> x = fmap f x]
   Empty <*> _ = Empty -- what do i apply?
   _ <*> Empty = Empty -- don't have anything to apply the functor on
   (Value f ftail) <*> (Value a tail) = Value (f a) (ftail <*> tail)

-- a fistful of monads
data Validation a = Success a | Fail String deriving (Show)

-- make validation an instance of functor type class
instance Functor Validation where
   fmap f (Success x) = Success (f x)
   fmap f (Fail str) = Fail str

-- make validation an instance of applicative functor
instance Applicative Validation where
   pure = Success
   Fail str <*> _ = Fail str
   Success f <*> lhs = fmap f lhs

-- make Validation an instance of Monad
instance Monad Validation where
   return x = Success x
   Fail str >>= _ = Fail str
   Success x >>= f = f x
   fail msg = Fail msg

positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x = if x > 0 then Success x else Fail ("negative!")

evenCheck ::(Integral a) => a -> Validation a
evenCheck x = if x `mod` 2 == 0 then Success x else Fail ("Odd!")

positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
-- positiveAndEvenCheck x = do
--    y <- positiveCheck x
--    z <- evenCheck y
--    return z
positiveAndEvenCheck = (\x -> positiveCheck x >>= evenCheck)

-- for a few monads
describe :: (Show a, Eq a, Ord a) => a -> a -> [String]
describe x y
   | x < y = [show x ++ " is less than " ++ show y]
   | x > y = [show x ++ " is greater than " ++ show y]
   | otherwise = [show x ++ " is equal to " ++ show y]

binarySearch :: (Show a, Eq a, Ord a) => (a -> a -> b) -> a -> [a] -> Writer [String] Bool
binarySearch _ _ [] = do
   return False
binarySearch comp elem [x] -- single element
   | elem == x = do
      tell $ describe elem x
      return True
   | otherwise = do
      tell $ describe elem x
      return False
binarySearch comp elem list
   | elem == middle = do
      tell $ describe elem middle
      return True
   | elem < middle = do
      tell $ describe elem middle
      binarySearch comp elem frontList
   | otherwise = do
      tell $ describe elem middle
      binarySearch comp elem backlist
   where
   size = fromIntegral $ length list
   middleIdx = floor ( size / 2 )
   middle = list !! middleIdx
   frontList = take middleIdx list
   backlist = drop (middleIdx + 1) list
