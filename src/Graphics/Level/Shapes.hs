module Graphics.Level.Shapes where

import Graphics.Level.Types

import Linear
import Linear.Affine
import Numeric.Interval.Kaucher

import Control.Applicative
import Control.Lens ((^.))


sphere :: Implicit
sphere = Implicit (\p -> {-# SCC "sphereFun" #-} qdA p origin - 1) (\_ -> {-# SCC "sphereEnv" #-} -1 ... 1)

box :: Implicit
box = Implicit
      (\p -> maximum [p^._x - 1, p^._y - 1, p^._z - 1, -1 - p^._x, -1 - p^._y, -1 - p^._z])
      (boxExtents $ V3 1.1 1.1 1.1)

boxExtents :: Vec -> Vec -> Interval Double
boxExtents (V3 xl yl zl) (V3 x y z) = -d ... d where
  d = maximum [xl // x, yl // y, zl // z]
  a // b | b == 0 = 0
  a // b = a / b

-- | @genCyl rMax f@ is a generalized cylinder defined by a radius
-- function @f@.  More precisely, for z = [0,1], it is the union of
-- circles of radius @f z@ perpendicular to the z-axis and centered at
-- [0,0,z].  @rMax@ is the maximum value of @f@ in the domain, and is
-- needed to construct the bounding envelope.
genCyl :: Double -> (Double -> Double) -> Implicit
genCyl rMax f = Implicit im (boxExtents $ V3 rMax rMax 1) where
  im :: Pt -> Double
  im p = maximum [radial, z - 1, -z] where
    radial = sqrt (quadrance v - z ^ (2 :: Int)) - f z
    v = p .-. origin
    z  = p ^. _z
