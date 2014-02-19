import Graphics.Level.Polygonization
import Graphics.Formats.STL.Printer
import Text.PrettyPrint
import Linear
import Linear.Affine

sphere = Implicit (\p -> qdA p origin - 1) (\_ -> V2 (-1) 1)

main = putStrLn . render . prettySTL . polygonize 0.01 $ sphere
