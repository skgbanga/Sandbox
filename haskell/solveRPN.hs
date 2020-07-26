solveRPN :: String -> Int
solveRPN exp = head (solveRPN' (words exp) [])

operate :: (Int -> Int -> Int) -> [Int] -> [Int]
operate func (x:y:xs) = (func y x):xs

solveRPN' :: [String] -> [Int] -> [Int]
solveRPN' [] st = st
solveRPN' (x:xs) st
   | x == "+" = solveRPN' xs (operate (+) st)
   | x == "-" = solveRPN' xs (operate (-) st)
   | x == "*" = solveRPN' xs (operate (*) st)
   | otherwise = solveRPN' xs ((read x :: Int):st)

solveRPN1 :: (Num a, Read a) => String -> a
solveRPN1 exp = head $ foldl someFunc [] (words exp)
   where someFunc (x:y:xs) "+" = (x + y):xs
         someFunc (x:y:xs) "-" = (y - x):xs
         someFunc (x:y:xs) "*" = (x * y):xs
         someFunc a s = (read s):a
