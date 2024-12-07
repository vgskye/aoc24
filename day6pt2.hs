import System.IO
import Control.Monad
import Data.List
import Debug.Trace (trace)
import qualified Data.HashSet as HS
import Control.Parallel.Strategies (rpar, parMap)

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Eq, Enum)

turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp

nextPos :: (Int, Int) -> Direction -> (Int, Int)
nextPos (x, y) DirUp = (x, y - 1)
nextPos (x, y) DirRight = (x + 1, y)
nextPos (x, y) DirDown = (x, y + 1)
nextPos (x, y) DirLeft = (x - 1, y)

stepGuard :: HS.HashSet (Int, Int) -> (Int, Int) -> Direction -> ((Int, Int), Direction)
stepGuard obstacles (x, y) direction
    | nextPos (x, y) direction `HS.member` obstacles = ((x, y), turnRight direction)
    | otherwise = (nextPos (x, y) direction, direction)

willLoop :: (Int, Int) -> (Int, Int) -> Direction -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int) -> Bool
willLoop (w, h) (x, y) direction visited obstacles
    | x < 0 || x >= w || y < 0 || y >= h = False
    | otherwise =
        let (newPos, newDir) = stepGuard obstacles (x, y) direction in
        let newVisited = if direction /= newDir && newDir == DirRight
            then HS.insert newPos visited
            else visited
        in (direction /= newDir && newDir == DirRight && newPos `HS.member` visited)
        || willLoop (w, h) newPos newDir newVisited obstacles


collectPos (w, h) obstacles (x, y) direction
    | x < 0 || x >= w || y < 0 || y >= h = []
    | nextPos (x, y) direction `elem` obstacles = nextPos (x, y) direction : collectPos (w, h) obstacles (x, y) (turnRight direction)
    | otherwise =
        let (newPos, newDir) = stepGuard obstacles (x, y) direction
        in (x, y) : collectPos (w, h) obstacles newPos newDir

main :: IO ()
main = do
        handle <- openFile "day6.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let obstacles = concatMap (\i -> map (, i) (elemIndices '#' (origmap !! i))) [0..length origmap - 1]
        let guardPos = (head . concatMap (\i -> map (, i) (elemIndices '^' (origmap !! i)))) [0..length origmap - 1]
        let emptySquares = concatMap (\i -> map (, i) (elemIndices '.' (origmap !! i))) [0..length origmap - 1]
        let obstacleSet = HS.fromList obstacles
        let relevantSquares = collectPos (width, height) obstacleSet guardPos DirUp
        let toCheck = filter (`elem` relevantSquares) emptySquares
        let loopySquares = parMap rpar (willLoop (width, height) guardPos DirUp HS.empty . (`HS.insert` obstacleSet)) toCheck
        print ((length . filter id) loopySquares)
        hClose handle