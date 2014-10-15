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
box = Implicit (\p -> {-# SCC "boxFun" #-} maximum [p^._x - 1, p^._y - 1, p^._z - 1, -1 - p^._x, -1 - p^._y, -1 - p^._z])
               (\v -> {-# SCC "boxEnv" #-} (pure (-1.1) `dot` v) ... (pure 1.1 `dot` v))
       -- TODO require by contract that v is a unit vector
  -- where
  --   manhattan (V3 x y z) = x+y+z

-- | @genCyl rMax f@ is a generalized cylinder defined by a radius
-- function @f@.  More precisely, for z = [0,1], it is the union of
-- circles of radius @f z@ perpendicular to the z-axis and centered at
-- [0,0,z].  @rMax@ is the maximum value of @f@ in the domain, and is
-- needed to construct the bounding envelope.
-- genCyl :: (Double -> Double) -> Implicit
-- genCyl rMax f = Implicit im env where
--   im p = sqrt (dot v v) (dot v unitZ) - f (p ^. _z) where
--     v = p .-. origin
--     unitZ = V3 0 0 1
--   env = pure . dot (V3 rMax rMax 1)
