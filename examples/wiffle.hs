import Control.Applicative
import Control.Lens
import Graphics.Formats.STL.Binary
import Graphics.Level.Polygonization
import Linear
import Linear.Affine
import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Data.Serialize

sphere = Implicit (\p -> {-# SCC "sphereFun" #-} qdA p origin - 1) (\_ -> {-# SCC "sphereEnv" #-} V2 (-1) 1)

box = Implicit (\p -> {-# SCC "boxFun" #-} maximum [p^._x - 1, p^._y - 1, p^._z - 1, -1 - p^._x, -1 - p^._y, -1 - p^._z])
               (\v -> {-# SCC "boxEnv" #-} V2 (pure (-1.1) `dot` v) (pure 1.1 `dot` v))
       -- TODO require by contract that v is a unit vector
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

main = do
    args <- getArgs
    let
        eps :: Double
        eps = read . head $ args
    BS.writeFile "wiffle.stl" . runPut . put . polygonize eps $ difference box $ scale 1.2 sphere
