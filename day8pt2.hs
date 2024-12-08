import System.IO
import Control.Monad
import Data.List
import Combinatorics (variate)

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

calcAntinodes w h [(x1, y1), (x2, y2)]
    | x1 < 0 || x1 >= w || y1 < 0 || y1 >= h = [(x2, y2)]
    | otherwise =
        let dx = x1 - x2 in
        let dy = y1 - y2 in
            (x2, y2) : calcAntinodes w h [(x1 + dx, y1 + dy), (x1, y1)]

isInside w h (x, y) = x >= 0 && x < w && y >= 0 && y < h

main = do
        handle <- openFile "day8.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let frequencies = (nub . concatMap (concatMap (filter (isInside width height) . calcAntinodes width height) . variate 2 . findNodes origmap) . nub . sort . concatMap (filter ('.' /=))) origmap
        print (length frequencies)
        hClose handle