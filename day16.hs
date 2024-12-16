import System.IO
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Min as MQ
import Debug.Trace
import Data.Maybe
import Control.Parallel.Strategies

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

data QueueEntry = QueueEntry Integer (Int, Int) Direction deriving (Eq, Show)

instance Ord QueueEntry where
    compare (QueueEntry dist1 _ _) (QueueEntry dist2 _ _) = compare dist1 dist2

djikstra walls target visited tovisit
    | (target, fromEnum DirUp) `HM.member` visited = let Just v = HM.lookup (target, fromEnum DirUp) visited in v
    | (target, fromEnum DirRight) `HM.member` visited = let Just v = HM.lookup (target, fromEnum DirRight) visited in v
    | (target, fromEnum DirDown) `HM.member` visited = let Just v = HM.lookup (target, fromEnum DirDown) visited in v
    | (target, fromEnum DirLeft) `HM.member` visited = let Just v = HM.lookup (target, fromEnum DirLeft) visited in v
    | otherwise = do
        let Just (QueueEntry dist pos dir, newTovisit) = MQ.minView tovisit
        if (pos `HS.member` walls) || ((pos, fromEnum dir) `HM.member` visited)
            then djikstra walls target visited newTovisit
            else do
                let newVisited = HM.insert (pos, fromEnum dir) dist visited
                let newerTovisit = if not ((nextPos pos dir, fromEnum dir) `HM.member` newVisited || nextPos pos dir `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos dir) dir) newTovisit
                    else newTovisit
                let newererTovisit = if not ((pos, fromEnum (turnRight dir)) `HM.member` newVisited)
                    then MQ.insert (QueueEntry (dist + 1000) pos (turnRight dir)) newerTovisit
                    else newerTovisit
                let newerererTovisit = if not ((pos, fromEnum (turnLeft dir)) `HM.member` newVisited)
                    then MQ.insert (QueueEntry (dist + 1000) pos (turnLeft dir)) newererTovisit
                    else newererTovisit
                djikstra walls target newVisited newerererTovisit
main = do
        handle <- openFile "day16.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let walls = findNodes origmap '#'
        let [start] = findNodes origmap 'S'
        let [end] = findNodes origmap 'E'
        let scored = djikstra (HS.fromList walls) end HM.empty (MQ.singleton (QueueEntry 0 start DirRight))
        print scored
        hClose handle