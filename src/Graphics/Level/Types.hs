module Graphics.Level.Types where

import Linear
import Linear.Affine
import Numeric.Interval.Kaucher

-- TODO generalize data types; keeping them simple to speed prototyping
type Pt = Point V3 Double
type Vec = V3 Double

-- TODO try using AD for the normal vectors, and for polygonization
-- algorithms which depend on moving points along the function
-- gradient.  For now, just take centered differences.

-- | An Implicit solid / surface is a Real-valued function over 3D
-- space The surface is the set of points where this function is 0 (A
-- level set).  The function is negative "inside" the solid, and
-- positive "outside".
--
-- The second argument gives a bound on the size of the solid in the
-- given direction.  This need not be a tight bound.  This allows
-- combining bounds in a simple way, while stil providing reasonable
-- limits during polygonization or raytracing.  Bounding boxes (or
-- spheres) could be used instead, provided they are not axis-aligned.
data Implicit = Implicit (Pt -> Double) (Vec -> Interval Double)
