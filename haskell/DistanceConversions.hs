module DistanceConversions
(
   inToCm,
   chToM
) where

inToCm :: Float -> Float
inToCm = (* 2.54)

chToM :: Float -> Float
chToM = (* 20.1168)
