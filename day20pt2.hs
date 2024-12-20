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

data QueueEntry = QueueEntry Int (Int, Int) deriving (Eq, Show)

instance Ord QueueEntry where
    compare (QueueEntry dist1 _) (QueueEntry dist2 _) = compare dist1 dist2



djikstra w h walls visited tovisit
    | MQ.null tovisit = visited
    | otherwise = do
        let Just (QueueEntry dist pos, newTovisit) = MQ.minView tovisit
        if pos `HM.member` visited
            then djikstra w h walls visited newTovisit
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
                djikstra w h walls newVisited newererererTovisit

unwrap (Just a) = a
unwrap Nothing = error "ohno"

main = do
    handle <- openFile "day20.txt" ReadMode
    origmap <- fmap lines (hGetContents handle)
    let width = (length . head) origmap
    let height = length origmap
    let walls = findNodes origmap '#'
    let wallset = HS.fromList walls
    let [start] = findNodes origmap 'S'
    let [end] = findNodes origmap 'E'
    let startDists = djikstra width height wallset HM.empty (MQ.singleton (QueueEntry 0 start))
    let endDists = djikstra width height wallset HM.empty (MQ.singleton (QueueEntry 0 end))
    let Just legitDist = HM.lookup end startDists
    let allPos = concatMap (\e -> map (,e) [0..width - 1]) [0..height - 1]
    let allReachable = filter (`HM.member` startDists) allPos
    let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    let jumpDist pos1 pos2 = unwrap (HM.lookup pos1 startDists) + manhattan pos1 pos2 + unwrap (HM.lookup pos2 endDists)
    let allCheats = concatMap (\e -> map (,e) allReachable) allReachable
    let possibleCheats = filter ((<= 20) . uncurry manhattan) allCheats
    let goodCheats = filter ((<= legitDist - 100) . uncurry jumpDist) possibleCheats
    print (length goodCheats)
    hClose handle