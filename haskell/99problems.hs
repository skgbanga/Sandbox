-- 99problems
-- last element of the list
mylast xs = head (reverse xs)

-- myBustLast
myBustLast xs = last (init xs)

-- Find the kth element
myElement xs k = last (take k xs)

-- Reverse a list
rev xs = if null xs then xs else rev (tail xs) ++ [ head xs ]

-- number of elements in the list
myLength xs = sum [1 | _<-xs]

-- compress a list
-- compress :: (Eq a) => [a] -> [a]
-- compress [] = []
-- compress [x] = [x]
-- compress (x:y:xs) = if x == y then compress (x:xs) else x:(compress (y:xs))

compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

-- great solution!
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)
