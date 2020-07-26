{-# OPTIONS_GHC -Wall #-}

-- typedefs
type Peg = String
type Move = (Peg, Peg)

-- The following function takes number of disks, and names of the three pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)
