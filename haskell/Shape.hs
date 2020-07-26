data Point = Point Float Float
data Shape = Circle Point Float | Rectangle Point Point

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = h * b
   where h = abs $ y1 - y2
         b = abs $ x1 - x2

center :: Shape -> Point
center (Circle p r) = p

diagonal :: Shape -> Float
diagonal (Rectangle (Point x1 y1) (Point x2 y2)) = sqrt $ h ^ 2 + b ^ 2
   where h = abs $ y1 - y2
         b = abs $ x1 - x2

data Person = Person { first :: String
                     , second :: String
                     } deriving (Show)
