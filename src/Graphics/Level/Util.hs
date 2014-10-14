module Graphics.Level.Util where

import Graphics.Level.Types

import Linear
import Linear.Affine

import Control.Applicative
import Control.Lens

translate :: Vec -> Implicit -> Implicit
translate v (Implicit f env) = Implicit (f . (.-^ v)) (\u -> pure (v `dot` normalize u) + env u)

intersect :: Implicit -> Implicit -> Implicit
intersect (Implicit f env1) (Implicit g env2) = Implicit fg env where
  fg p = max (f p) (g p)
  env p = min <$> env1 p <*> env2 p

union :: Implicit -> Implicit -> Implicit
union (Implicit f env1) (Implicit g env2) = Implicit fg env where
  fg p = min (f p) (g p)
  env p = max <$> env1 p <*> env2 p

scale :: Double -> Implicit -> Implicit
scale s (Implicit f env) = Implicit f' env' where
  f' p = f $ p & lensP . mapped //~ s
  env' p = (*s) <$> env p

-- difference can have a tighter bound than union
-- negate has an infinite bound, so we don't do that to Implicits
difference :: Implicit -> Implicit -> Implicit
difference (Implicit f env1) (Implicit g _) = Implicit fg env1 where
  fg p = max (f p) (negate (g p))
