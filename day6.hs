import System.IO
import Control.Monad
import Data.List

data Direction = DirUp | DirRight | DirDown | DirLeft

turnRight DirUp = DirRight
turnRight DirRight = DirDown
turnRight DirDown = DirLeft
turnRight DirLeft = DirUp

nextPos (x, y) DirUp = (x, y - 1)
nextPos (x, y) DirRight = (x + 1, y)
nextPos (x, y) DirDown = (x, y + 1)
nextPos (x, y) DirLeft = (x - 1, y)

stepGuard obstacles (x, y) direction
    | nextPos (x, y) direction `elem` obstacles = stepGuard obstacles (x, y) (turnRight direction)
    | otherwise = (nextPos (x, y) direction, direction)

collectPos (w, h) obstacles (x, y) direction
    | x < 0 || x >= w || y < 0 || y >= h = []
    | otherwise =
        let (newPos, newDir) = stepGuard obstacles (x, y) direction
        in (x, y) : collectPos (w, h) obstacles newPos newDir

main = do
        handle <- openFile "day6.txt" ReadMode
        origmap <- fmap lines (hGetContents handle)
        let width = (length . head) origmap
        let height = length origmap
        let obstacles = concatMap (\i -> map (, i) (elemIndices '#' (origmap !! i))) [0..length origmap - 1]
        let guardPos = (head . concatMap (\i -> map (, i) (elemIndices '^' (origmap !! i)))) [0..length origmap - 1]
        let positions = nub (collectPos (width, height) obstacles guardPos DirUp)
        print (length positions)
        hClose handle