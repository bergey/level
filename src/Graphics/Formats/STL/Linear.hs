module Graphics.Formats.STL.Linear where

import Control.Applicative
import Control.Lens (view)
import Graphics.Formats.STL
import Linear
import Linear.Affine

triangle :: V3 Double -> V3 (Point V3 Double) -> Triangle
triangle n pts = Triangle (asTriple n) . asTriple $ (asTriple . view lensP <$> pts)

asTriple :: V3 a -> (a,a,a)
asTriple (V3 x y z) = (x,y,z)

asV3 :: (a,a,a) -> V3 a
asV3 (x,y,z) = V3 x y z
