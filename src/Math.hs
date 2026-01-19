module Math where

import Data.Ord (clamp)
import Graphics.Gloss (Vector, Point)

import Graphics.Gloss.Data.Point.Arithmetic qualified as P (
  (*),
  (-),
 )

scalarProduct :: Vector -> Vector -> Float
scalarProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

getNormVector :: Vector -> Vector
getNormVector v = (1 / sqrt (scalarProduct v v)) P.* v

betweenValues :: Float -> Float -> Float -> Float
betweenValues lowlim value uplim = clamp (lowlim, uplim) value

resizeVectorFactor :: Float -> Float -> Vector -> Float
resizeVectorFactor lowlim uplim v = betweenValues lowlim (sqrt (scalarProduct v v)) uplim

maxJumpDistance :: Float -> Float
-- 500 time steps are needed decrease velocity to 1 percent of its initial value
maxJumpDistance maxV = 0.5 * 500 * maxV

smoothstep :: Float -> Float
smoothstep t =
  let
    x = clamp (0, 1) t
  in
    x * x * x * (x * (x * 6 - 15) + 10)

smoothTransition
  :: Point
  -- ^ startPoint
  -> Point
  -- ^ endPoint
  -> Float
  -- ^ x
  -> Float
  -- ^ y
smoothTransition (x1, y1) (x2, y2) x =
  let
    t = (x - x1) / (x2 - x1)
  in
    y1 + (y2 - y1) * smoothstep t
