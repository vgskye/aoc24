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

data QueueEntry = QueueEntry Integer (Int, Int) Bool deriving (Eq, Show)

instance Ord QueueEntry where
    compare (QueueEntry dist1 _ _) (QueueEntry dist2 _ _) = compare dist1 dist2



djikstra w h walls target visited tovisit cheat maxdist
    | target `HM.member` visited = visited
    | otherwise = do
        let Just (QueueEntry dist pos cheated, newTovisit) = MQ.minView tovisit
        let (x, y) = pos
        if maxdist < dist then visited
        else if (pos `HM.member` visited) || (x >= w) || (x < 0) || (y >= h) || (y < 0)
            then djikstra w h walls target visited newTovisit cheat maxdist
            else do
                let newVisited = HM.insert pos dist visited
                let newerTovisit = if not (nextPos pos DirUp `HM.member` newVisited || nextPos pos DirUp `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirUp) cheated) newTovisit
                    else newTovisit
                let newererTovisit = if not (nextPos pos DirRight `HM.member` newVisited || nextPos pos DirRight `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirRight) cheated) newerTovisit
                    else newerTovisit
                let newerererTovisit = if not (nextPos pos DirDown `HM.member` newVisited || nextPos pos DirDown `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirDown) cheated) newererTovisit
                    else newererTovisit
                let newererererTovisit = if not (nextPos pos DirLeft `HM.member` newVisited || nextPos pos DirLeft `HS.member` walls)
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirLeft) cheated) newerererTovisit
                    else newerererTovisit
                let newerererererTovisit = if not (nextPos pos DirUp `HM.member` newVisited || cheated) && (nextPos pos DirUp, DirUp) == cheat
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirUp) True) newererererTovisit
                    else newererererTovisit
                let newererererererTovisit = if not (nextPos pos DirRight `HM.member` newVisited || cheated) && (nextPos pos DirRight, DirRight) == cheat
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirRight) True) newerererererTovisit
                    else newerererererTovisit
                let newerererererererTovisit = if not (nextPos pos DirDown `HM.member` newVisited || cheated) && (nextPos pos DirDown, DirDown) == cheat
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirDown) True) newererererererTovisit
                    else newererererererTovisit
                let newererererererererTovisit = if not (nextPos pos DirLeft `HM.member` newVisited || cheated) && (nextPos pos DirLeft, DirLeft) == cheat
                    then MQ.insert (QueueEntry (dist + 1) (nextPos pos DirLeft) True) newerererererererTovisit
                    else newerererererererTovisit
                djikstra w h walls target newVisited newererererererererTovisit cheat maxdist
optimalpaths targetDist target pathing pos lastDist trail
    | targetDist < lastDist = HS.empty
    | target == pos && lastDist == targetDist = HS.fromList (pos:trail)
    | target == pos = HS.empty
    | otherwise =
        case HM.lookup pos pathing of
            Nothing -> HS.empty
            Just dist ->
                if lastDist /= dist
                    then HS.empty
                    else HS.unions [
                        optimalpaths targetDist target pathing (nextPos pos DirUp) (dist + 1) (pos:trail),
                        optimalpaths targetDist target pathing (nextPos pos DirRight) (dist + 1) (pos:trail),
                        optimalpaths targetDist target pathing (nextPos pos DirDown) (dist + 1) (pos:trail),
                        optimalpaths targetDist target pathing (nextPos pos DirLeft) (dist + 1) (pos:trail)
                        ]

unwrap (Just a) = a
unwrap Nothing = 999999

main = do
        handle <- openFile "day20.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let walls = findNodes origmap '#'
        let wallset = HS.fromList walls
        let [start] = findNodes origmap 'S'
        let [end] = findNodes origmap 'E'
        let scorepath = djikstra width height wallset end HM.empty (MQ.singleton (QueueEntry 0 start True)) ((0, 0), DirUp) 99999999
        let Just scored = HM.lookup end scorepath
        let scoreWithC cheat = scored - 100 >= unwrap (HM.lookup end (djikstra width height wallset end HM.empty (MQ.singleton (QueueEntry 0 start False)) cheat (scored - 100)))
        let isViable (x, y) = x /= 0 && y /= 0 && x /= (width - 1) && y /= (height - 1)
        let allpaths = optimalpaths scored end scorepath start 0 []
        let viableWalls = filter isViable walls
        let cheats = concatMap (\e -> map (e,) [DirUp, DirRight, DirDown, DirLeft]) viableWalls
        let viableCheats = filter (\(p, dir) -> (nextPos p (turnRight . turnRight $ dir) `HS.member` allpaths) && not (nextPos p dir `HS.member` wallset || nextPos p (turnRight . turnRight $ dir) `HS.member` wallset)) cheats
        print (length (filter scoreWithC viableCheats))
        hClose handle