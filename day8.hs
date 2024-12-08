import System.IO
import Control.Monad
import Data.List
import Combinatorics (variate)

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

calcAntinode [(x1, y1), (x2, y2)] =
    let dx = x1 - x2 in
    let dy = y1 - y2 in
        (x1 + dx, y1 + dy)

isInside w h (x, y) = x >= 0 && x < w && y >= 0 && y < h

main = do
        handle <- openFile "day8.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let frequencies = (nub . concatMap (filter (isInside width height) . map calcAntinode . variate 2 . findNodes origmap) . nub . sort . concatMap (filter ('.' /=))) origmap
        print (length frequencies)
        hClose handle