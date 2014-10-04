module Graphics.Formats.STL.Linear where

import Control.Applicative
import Control.Lens (view)
import Graphics.Formats.STL
import Linear
import Linear.Affine

triangle :: Real a => Maybe (V3 a) -> V3 (Point V3 a) -> Triangle
triangle n pts = Triangle (asVector <$> n) . asTriple $ (asVector . view lensP <$> pts)

asTriple :: V3 a -> (a,a,a)
asTriple (V3 x y z) = (x,y,z)

asVector :: Real a => V3 a -> (Float,Float,Float)
asVector (V3 x y z) = (r2f x,r2f y,r2f z)

asV3 :: (a,a,a) -> V3 a
asV3 (x,y,z) = V3 x y z

r2f :: (Real r, Fractional f) => r -> f
r2f = realToFrac
