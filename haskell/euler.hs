-- #1
sumMult3_5 :: Int -> Int
sumMult3_5 n = sum $ filter func [1..(n - 1)]
   where
   func k
      | (k `mod` 5) == 0 = True
      | (k `mod` 3) == 0 = True
      | otherwise = False

-- #2
sum_even_fib :: (Integral a, Ord a) => a -> a
sum_even_fib n = sum $ takeWhile (< n) $ filter even fib_list
   where fib_list =  1 : 2 : (zipWith (+) fib_list (tail fib_list))

-- #3
-- VERY SLOW!
is_prime :: (Integral a) => a -> Bool
is_prime n = length results == 0
   where root = sqrt $ fromIntegral n
         fl   = floor root
         candidates = [2..fl]
         results = filter (\x -> n `mod` x == 0) candidates

largest_prime :: (Integral a) => a -> a
largest_prime n = head $ filter (\x -> is_prime x && (n `mod` x) == 0) [n, n-1 .. 2]
