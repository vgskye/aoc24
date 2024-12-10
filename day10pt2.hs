import System.IO
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import Debug.Trace

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

cardinals = [(1, 0), (-1, 0), (0, -1), (0, 1)]

offset (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

findPeaks :: [HS.HashSet (Int, Int)] -> (Int, Int) -> Int
findPeaks [set] (x, y) = if HS.member (x, y) set then 1 else 0
findPeaks (set:xs) (x, y) = if HS.member (x, y) set then (sum . map (findPeaks xs . ((x, y) `offset`))) cardinals else 0

main = do
        handle <- openFile "day10.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let frequencies = map (HS.fromList . findNodes origmap) ['0'..'9']
        let peaks = (sum . map (findPeaks frequencies) . HS.toList . head) frequencies
        print peaks
        hClose handle