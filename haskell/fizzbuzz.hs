import Data.List (intercalate)

fizzbuzz :: Int -> String
fizzbuzz x
   | x `mod` 15 == 0 = "FizzBuzz"
   | x `mod` 5  == 0 = "Fizz"
   | x `mod` 3  == 0 = "Buzz"
   | otherwise = show x

-- main = do
--    let str = intercalate " " $ map fizzbuzz [1..100]
--    putStrLn str

-- main = do
--    sequence $ map (print . fizzbuzz) [1..100]

main = mapM (print . fizzbuzz) [2..100]
