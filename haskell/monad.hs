import Control.Monad.Trans.State
import System.Random

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \s -> ((), (a:s))

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

-- state takes a transformation
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
   x <- randomSt
   y <- randomSt
   z <- randomSt
   return (x, y, z)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
   | f x = x : xs'
   | otherwise = xs'
   where xs' = filter' f xs

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = return []
filterM' f (x:xs) = do
   b <- f x
   if b
      then fmap (x:) rest
      else rest
   where rest = filterM' f xs
