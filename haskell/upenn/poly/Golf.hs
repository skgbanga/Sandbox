{-# OPTIONS_GHC -Wall #-}
module Golf where

import qualified Data.Map as Map
import qualified Data.List as List
{-
 - Create a list of functions. This functions take a number and (index, value) pair and do
 - index `mod` value to get a boolean filter. This filter is then applied on the argument list
 - to get the value for all the indices present in the list
-}
skips :: [a] -> [[a]]
skips xs = take (length xs) $ pairs xs
   where funcs = zipWith ($) (iterate id createFilter) [1..]
         pairs l = map (\f -> map snd $ filter f $ zip [1..] l) funcs

createFilter :: Int -> (Int, a) -> Bool
createFilter k (n, _) = n `mod` k == 0

{-
 - Local maximas
 - Straightforward implementation - zip3 would have been better
 -}

localMaxima :: [Integer] -> [Integer]
localMaxima [] = [];
localMaxima [_] = [];
localMaxima [_, _] = [];
localMaxima (x:y:z:xs) = if (y > z && y > x) then [y] ++ localMaxima (y:z:xs) else localMaxima (y:z:xs)

{-
 - histogram of numbers between 0 and 9
 - ugliest haskell function ever!
 -}

histogram :: [Int] -> String
histogram xs = (List.intercalate "\n" trans) ++ suffix
   where map_ = Map.fromList $ map (\x -> (x, 0)) [0..9]
         umap = foldr (\x m -> Map.adjust succ x m) map_ xs
         lists = map (flip replicate '*') $ Map.elems umap
         max_ = foldr (\str y -> max (length str) y) 0 lists
         extend str n = let k = n - (length str) in str ++ (replicate k ' ')
         trans = reverse $ List.transpose $ map (flip extend max_) lists
         suffix = "\n==========\n0123456789\n"
