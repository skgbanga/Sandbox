{-# OPTIONS_GHC -Wall #-}

import qualified Data.Char as Char

{- 
- Exercises 1-4: validate a credit card number
-}

-- toDigits returns [] on non-positive numbers
toDigits :: Integer -> [Integer]
toDigits x
   | x <= 0 = []
   | otherwise = map (toInteger . Char.digitToInt) $ show x

-- same as the above but with reverse list out of the numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- enumerate xs gives a sequence of (index, value) from reverse
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) (cycle [1, 2]) xs

-- ultra meta
-- doubleEveryOther xs = reverse $ zipWith ($) (cycle [id, (*2)]) (reverse $)

-- doubleEveryOther xs = reverse $ map (\(i, v) -> if (i `mod` 2 /= 0) then 2 * v else v) $ enumerate xs
--    where enumerate xs = zip [0..] $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- Final function - validate the credit card number
validate :: Integer -> Bool
validate x = (transform x) `mod` 10 == 0
   where transform = sumDigits . doubleEveryOther . toDigitsRev
