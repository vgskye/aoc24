import System.IO
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Min as MQ
import Debug.Trace
import Data.Maybe
import Control.Parallel.Strategies
import Data.List.Split (splitOn)

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Eq, Enum, Show)

turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp

turnLeft DirUp = DirLeft
turnLeft DirRight = DirUp
turnLeft DirDown = DirRight
turnLeft DirLeft = DirDown

nextPos :: (Int, Int) -> Direction -> (Int, Int)
nextPos (x, y) DirUp = (x, y - 1)
nextPos (x, y) DirRight = (x + 1, y)
nextPos (x, y) DirDown = (x, y + 1)
nextPos (x, y) DirLeft = (x - 1, y)

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

minimumMaybe [] = Nothing
minimumMaybe l = Just (minimum l)

data QueueEntry = QueueEntry Integer (Int, Int) deriving (Eq, Show)

instance Ord QueueEntry where
    compare (QueueEntry dist1 _) (QueueEntry dist2 _) = compare dist1 dist2



djikstra w h walls target visited tovisit
    | target `HM.member` visited = let Just v = HM.lookup target visited in v
    | otherwise = do
        let Just (QueueEntry dist pos, newTovisit) = MQ.minView tovisit
        let (x, y) = pos
        if (pos `HS.member` walls) || (pos `HM.member` visited) || (x >= w) || (x < 0) || (y >= h) || (y < 0)
            then djikstra w h walls target visited newTovisit
            else do
                let newVisited = HM.insert pos dist visited
                let newerTovisit = if not (nextPos pos DirUp `HM.member` newVisited || nextPos pos DirUp `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirUp)) newTovisit
                    else newTovisit
                let newererTovisit = if not (nextPos pos DirRight `HM.member` newVisited || nextPos pos DirRight `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirRight)) newerTovisit
                    else newerTovisit
                let newerererTovisit = if not (nextPos pos DirDown `HM.member` newVisited || nextPos pos DirDown `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirDown)) newererTovisit
                    else newererTovisit
                let newererererTovisit = if not (nextPos pos DirLeft `HM.member` newVisited || nextPos pos DirLeft `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirLeft)) newerererTovisit
                    else newerererTovisit
                djikstra w h walls target newVisited newererererTovisit
main = do
        handle <- openFile "day18.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = 71
        let height = 71
        let walls = take 1024 . map ((\[a, b] -> (a, b)) . map read . splitOn ",") $ origmap
        let scored = djikstra width height (HS.fromList walls) (width - 1, height - 1) HM.empty (MQ.singleton (QueueEntry 0 (0, 0)))
        print scored
        hClose handle