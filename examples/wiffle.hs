import           Graphics.Formats.STL ()
import           Graphics.Level.Polygonization
import           Graphics.Level.Util
import           Graphics.Level.Shapes

import qualified Data.ByteString.Lazy as BS
import           Data.Serialize
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let
        eps :: Double
        eps = read . head $ args
    BS.writeFile "wiffle.stl" . encodeLazy . polygonize eps $ difference box $ scale 1.2 sphere
