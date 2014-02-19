import Control.Applicative
import Graphics.Formats.STL.Printer
import Graphics.Level.Polygonization
import Linear
import Linear.Affine
import Text.PrettyPrint

sphere = Implicit (\p -> qdA p origin - 1) (\_ -> V2 (-1) 1)
--sphere2 = Implicit (\p -> qdA p (pure 0.5) - 1) (\v -> V2 (-1) 1 + pure (pure 0.5 `dot` normalize v))
sphere2 = translate (pure 0.5) sphere

translate :: Vec -> Implicit -> Implicit
translate v (Implicit f env) = Implicit (f . (.-^ v)) (\u -> pure (v `dot` normalize u) + env u)

intersect (Implicit f env1) (Implicit g env2) = Implicit fg env where
  fg p = max (f p) (g p)
  env p = V2 (min l1 l2) (min u1 u2) where
    V2 l1 u1 = env1 p
    V2 l2 u2 = env2 p

main = putStrLn . render . prettySTL . polygonize 0.1 $ intersect sphere sphere2
