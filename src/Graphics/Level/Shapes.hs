module Graphics.Level.Shapes where

import Graphics.Level.Types

import           Linear
import           Linear.Affine

import           Control.Applicative
import           Control.Lens


sphere :: Implicit
sphere = Implicit (\p -> {-# SCC "sphereFun" #-} qdA p origin - 1) (\_ -> {-# SCC "sphereEnv" #-} V2 (-1) 1)

box :: Implicit
box = Implicit (\p -> {-# SCC "boxFun" #-} maximum [p^._x - 1, p^._y - 1, p^._z - 1, -1 - p^._x, -1 - p^._y, -1 - p^._z])
               (\v -> {-# SCC "boxEnv" #-} V2 (pure (-1.1) `dot` v) (pure 1.1 `dot` v))
       -- TODO require by contract that v is a unit vector
  -- where
  --   manhattan (V3 x y z) = x+y+z
