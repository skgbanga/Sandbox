path :: [Int] -> [Int] -> [Int] -> (Int, Int)
path xs ys zs = path1 (reverse xs) (reverse ys) (reverse zs)

path1 :: [Int] -> [Int] -> [Int] -> (Int, Int)
path1 [x] [y] [z] = bestPair 0 0 x y z
path1 (x:xs) (y:ys) (z:zs) = bestPair bx bz x y z
   where (bx, bz) = path1 xs ys zs

bestPair :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
bestPair bx bz x y z = (min (bx + x) (bz + z + y), min (bz + z) (bx + x + y))
