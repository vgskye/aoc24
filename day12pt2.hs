import System.IO
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import Debug.Trace

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

cardinals = [(1, 0), (-1, 0), (0, -1), (0, 1)]

offset (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

flood :: (Int, Int) -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
flood (x, y) possibles visited
    | not ((x, y) `HS.member` possibles) = visited
    | (x, y) `HS.member` visited = visited
    | otherwise = foldr ((`flood` possibles) . offset (x, y)) (HS.insert (x, y) visited) cardinals

regionize :: HS.HashSet (Int, Int) -> [HS.HashSet (Int, Int)]
regionize set
    | HS.null set = []
    | otherwise = let flooded = flood ((head . HS.toList) set) set HS.empty
        in flooded : regionize (HS.difference set flooded)

hasSide (x, y) region (0, -1) = not (HS.member (x, y - 1) region) &&
                                not (HS.member (x - 1, y) region &&
                                    not (HS.member (x - 1, y - 1) region))
hasSide (x, y) region (0,  1) = not (HS.member (x, y + 1) region) &&
                                not (HS.member (x - 1, y) region &&
                                    not (HS.member (x - 1, y + 1) region))
hasSide (x, y) region (-1, 0) = not (HS.member (x - 1, y) region) &&
                                not (HS.member (x, y + 1) region &&
                                    not (HS.member (x - 1, y + 1) region))
hasSide (x, y) region ( 1, 0) = not (HS.member (x + 1, y) region) &&
                                not (HS.member (x, y + 1) region &&
                                    not (HS.member (x + 1, y + 1) region))

perimeter (x, y) region = (length . filter (hasSide (x, y) region)) cardinals

perimeterLen region = HS.foldr (\pos acc -> acc + (pos `perimeter` region)) 0 region

main = do
        handle <- openFile "day12.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let regions = (concatMap (regionize . HS.fromList . findNodes origmap) . nub . sort . concat) origmap
        print ((sum . map (\region -> HS.size region * perimeterLen region)) regions)
        hClose handle