module Math where

scalarProduct :: Vector -> Vector -> Float
scalarProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

getNormVector :: Vector -> Vector
getNormVector v = (1 / sqrt (scalarProduct v v)) P.* v

betweenValues :: Float -> Float -> Float -> Float
betweenValues lowlim value uplim = max lowlim (min value uplim)

resizeVectorFactor :: Float -> Float -> Vector -> Float
resizeVectorFactor lowlim uplim v = betweenValues lowlim (sqrt (scalarProduct v v)) uplim

