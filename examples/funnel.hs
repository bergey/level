import           Graphics.Formats.STL ()
import           Graphics.Level.Polygonization
import           Graphics.Level.Util
import           Graphics.Level.Shapes
import           Graphics.Level.Types

import qualified Data.ByteString.Lazy as BS
import           Data.Serialize
import           System.Environment (getArgs)

narrowDiameter, wideDiameter, height, wallThickness :: Double  -- mm
narrowDiameter = 54
wideDiameter = 120
height = 100
wallThickness = 2

funnel :: Implicit
funnel = difference outer inner
-- funnel = difference outer cyl where
--   cyl = translateZ (-10) . scaleZ 120 $ genCyl 25 (const 25)

outer :: Implicit
outer = scaleZ height $ sigmoid (narrowDiameter / 2) (wideDiameter / 2)

inner :: Implicit
inner = translateZ (-1) . scaleZ (height + 2) $ sigmoid
        ((narrowDiameter - wallThickness) / 2)
        ((wideDiameter - wallThickness) / 2)

sigmoid :: Double -> Double -> Implicit
sigmoid  rMin rMax = genCyl rMax r where
  -- The domain for interval is [0,1]
  -- Take erf over the domain [-3,3]
  -- and map to the output range [rMin, rMax]
  r z = rMin + (rMax - rMin) * s (10 * z - 5)
  s t = 1/ (1 + exp (-t) ) -- logistic curve

main :: IO ()
main = do
    args <- getArgs
    let
        eps :: Double
        eps = read . head $ args
    BS.writeFile "funnel.stl" . encodeLazy . polygonize eps $ funnel
