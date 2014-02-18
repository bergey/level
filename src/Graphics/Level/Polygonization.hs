{-# LANGUAGE OverloadedStrings #-}

module Graphics.Level.Polygonization where

import Control.Applicative
import Linear
import Linear.Affine

import Graphics.Formats.STL
import Graphics.Formats.STL.Linear


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
data Implicit = Implicit (Pt -> Double) (Vec -> V2 Double)

-- | A cube represented by two opposite corners
data Cube = Cube Pt Pt

mkCube :: V2 Vec -> Cube
mkCube (V2 l u) = Cube (P l) (P u)

-- | Any 4 points define a tetrahedron, provided they are not all coplanar.
data Tetrahedron = Tetra Pt Pt Pt Pt

-- | @polygonize e s@ generates an explicit triangle mesh on the
-- surface of s, using a grid of axis-aligned cubes of size e.
polygonize :: Double -> Implicit -> STL
polygonize e s = STL "" $ concatMap (findSurface s) (cells e s)

-- | @cells e imp@ is a list of tetrahedra which decompose the
-- bounding box of @imp@.
cells :: Double -> Implicit -> [Tetrahedron]
cells e (Implicit _ env) = concatMap (tetrahedra e) ps where
  Cube l u = axisAlignedBox env
  V3 sx sy sz = u .-. l
  ps = (l .+^) <$> (V3 <$> [0,e..sx] <*> [0,e..sy] <*> [0,e..sz])

-- A 5-decomposition is possible, but this is simpler.  The
-- 5-decomposition requires coordination between adjacent cubic
-- proto-cells to ensure they share edges.  The 6-decomposition also
-- results in Kuhn simplices, which can be subdivided into similar
-- tetrahedra.  [Bloomenthal 1997] This could be useful for adaptive
-- subdivision in the future.

-- | @tetrahedra e p@ gives 6 tetrahedra filling the cube between @p@
-- and @p + pure e@
tetrahedra :: Double -> Pt -> [Tetrahedron]
tetrahedra step p = [ Tetra a b c e
                    , Tetra b c e g
                    , Tetra b e f g
                    , Tetra b c d g
                    , Tetra b d f g
                    , Tetra d f g h
                    ] where
  [a, b, c, d, e, f, g, h] = (p .+^) <$> (V3 <$> [0, step] <*> [0, step] <*> [0,step])


-- | find a pair of vectors indicating the corners of the box
axisAlignedBox :: (Vec -> V2 Double) -> Cube
axisAlignedBox f  = mkCube . sum $ zipWith mul basis $ map f basis where
  mul v mags = (*^ v) <$> mags

-- | A surface can intersect a 0, 3, or 4 edges of a tetrahedron.
-- This is approximated by 0, 1, or 2 triangles.  The latter are
-- coplanar, but this information is not useful.
findSurface :: Implicit -> Tetrahedron -> [Triangle]
findSurface i@(Implicit f _) (Tetra a b c d) = case map ((>0) . f) [a,b,c,d] of
    [False,False,False,False] -> []
    [False,False,False,True ] -> [ tri (edge b d) (edge c d) (edge a d)]
    [False,False,True ,False] -> [ tri (edge a c) (edge c d) (edge b c)]
    [False,False,True ,True ] -> [ tri (edge a d) (edge b d) (edge b c)
                                 , tri (edge b c) (edge a c) (edge a d)]
    [False,True ,False,False] -> [ tri (edge a b) (edge a d) (edge a c)]
    [False,True ,False,True ] -> [ tri (edge a d) (edge a b) (edge b c)
                                 , tri (edge b c) (edge c d) (edge a d)]
    [False,True ,True ,False] -> [ tri (edge a b) (edge a c) (edge c d)
                                 , tri (edge c d) (edge b d) (edge a b)]
    [False,True ,True ,True ] -> [ tri (edge a b) (edge a c) (edge a d)]
    [True ,False,False,False] -> [ tri (edge a b) (edge a d) (edge a c)]
    [True ,False,False,True ] -> [ tri (edge a b) (edge b d) (edge c d)
                                 , tri (edge c d) (edge a c) (edge a b)]
    [True ,False,True ,False] -> [ tri (edge a b) (edge a d) (edge c d)
                                 , tri (edge c d) (edge b c) (edge a b)]
    [True ,False,True ,True ] -> [ tri (edge a b) (edge b d) (edge b c)]
    [True ,True ,False,False] -> [ tri (edge a d) (edge a c) (edge b c)
                                 , tri (edge b c) (edge b d) (edge a d)]
    [True ,True ,False,True ] -> [ tri (edge c d) (edge a c) (edge b c)]
    [True ,True ,True ,False] -> [ tri (edge b d) (edge a d) (edge c d)]
    [True ,True ,True ,True ] -> []
    where
     tri a b c = triangle n $ V3 a b c
     edge = findPoint 5 i
     n = V3 0 0 0 -- null normal, acceptable in STL

-- | @findPoint@ tries to find a zero of the implicit function on the
-- line segment between the given points.  It performs a binary
-- search, and the first argument is the maximum number of divisions
-- to attempt.  It is an error to call this function on two points
-- where the function has the same sign.
findPoint :: Int -> Implicit -> Pt -> Pt -> Pt
findPoint n (Implicit f _) p0 q0 = case compare (signum fp0) (signum fq0) of
    EQ -> error ("findPoint was called on "++show p0++" and "++show q0++" where the function has the same sign")
    LT -> go n f (p0, fp0) (q0, fq0)
    GT -> go n f (q0, fq0) (p0, fp0)
  where
    fp0 = f p0
    fq0 = f q0
    -- go requires fp < 0; fq > 0
    go 0 f (p, fp) (q, fq) = case compare (abs fp) (abs fq) of
        LT -> p
        GT -> q
        EQ -> (p+q)/2
    go n f (p, fp) (q, fq) = let mid = (p+q)/2
                                 fmid = f mid
                             in
                              case compare fmid 0 of
                                  LT -> go (n-1) f (mid,fmid) (q,fq)
                                  GT -> go (n-1) f (p,fp) (mid,fmid)
                                  EQ -> mid
