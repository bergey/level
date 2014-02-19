import Control.Applicative
import Control.Lens
import Graphics.Formats.STL.Printer
import Graphics.Level.Polygonization
import Linear
import Linear.Affine
import Text.PrettyPrint

sphere = Implicit (\p -> qdA p origin - 1) (\_ -> V2 (-1) 1)

box = Implicit (\p -> maximum [p^._x - 1, p^._y - 1, p^._z - 1, -1 - p^._x, -1 - p^._y, -1 - p^._z])
               (\_ -> V2 (-1.1) 1.1) -- TODO only works for coordinate axes!
  where
    manhattan (V3 x y z) = x+y+z

translate :: Vec -> Implicit -> Implicit
translate v (Implicit f env) = Implicit (f . (.-^ v)) (\u -> pure (v `dot` normalize u) + env u)

intersect (Implicit f env1) (Implicit g env2) = Implicit fg env where
  fg p = max (f p) (g p)
  env p = min <$> env1 p <*> env2 p

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

main = putStrLn . render . prettySTL . polygonize 0.01 $ difference box (scale 1.2 sphere)
