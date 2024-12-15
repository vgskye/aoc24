import System.IO
import Control.Monad
import Data.List
import Combinatorics (variate)
import Data.List.Split (splitOn)
import qualified Data.HashSet as HS
import Debug.Trace

data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Eq, Enum, Show)

nextPos :: (Int, Int) -> Direction -> (Int, Int)
nextPos (x, y) DirUp = (x, y - 1)
nextPos (x, y) DirRight = (x + 1, y)
nextPos (x, y) DirDown = (x, y + 1)
nextPos (x, y) DirLeft = (x - 1, y)

findNodes mapStr freq = concatMap (\i -> map (, i) (elemIndices freq (mapStr !! i))) [0..length mapStr - 1]

parseInsns [] = []
parseInsns ('^':xs) = DirUp : parseInsns xs
parseInsns ('>':xs) = DirRight : parseInsns xs
parseInsns ('v':xs) = DirDown : parseInsns xs
parseInsns ('<':xs) = DirLeft : parseInsns xs
parseInsns ('\n':xs) = parseInsns xs

findToPush :: (Int, Int) -> Direction -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
findToPush (x, y) dir boxes visited
    | (x, y) `HS.member` visited = HS.empty
    | nextPos (x, y) dir `HS.member` boxes = nextPos (x, y) dir `HS.insert` findToPush (nextPos (x, y) dir) dir boxes (HS.insert (x, y) visited) `HS.union` findToPush (nextPos (x + 1, y) dir) dir boxes (HS.insert (x, y) visited)
    | nextPos (x - 1, y) dir `HS.member` boxes = nextPos (x - 1, y) dir `HS.insert` findToPush (nextPos (x, y) dir) dir boxes (HS.insert (x, y) visited) `HS.union` findToPush (nextPos (x - 1, y) dir) dir boxes (HS.insert (x, y) visited)
    | otherwise = HS.empty

findToPushReal pos dir boxes = findToPush pos dir boxes HS.empty

canMove walls pos dir boxes =
    let moved = HS.map (`nextPos` dir) boxes in
    let movedRights = HS.map (`nextPos` DirRight) moved in
    HS.null (walls `HS.intersection` (nextPos pos dir `HS.insert` moved `HS.union` movedRights))

pultimush walls pos dir boxes
    | canMove walls pos dir (findToPushReal pos dir boxes) =
        let toPush = findToPushReal pos dir boxes in
        (nextPos pos dir, HS.map (`nextPos` dir) toPush `HS.union` HS.difference boxes toPush)
    | otherwise = (pos, boxes)

replay [] walls pos boxes = (pos, boxes)
replay (dir:xs) walls pos boxes =
    let (newPos, newBoxes) = pultimush walls pos dir boxes in
        replay xs walls newPos newBoxes

vizmap w h walls boxes pos = concatMap (vizrow w walls boxes pos) [0..h - 1]
    where
        vizrow w walls boxes pos y = '\n':map (vizpoint walls boxes pos y) [0..w - 1]
        vizpoint walls boxes pos y x
            | (x, y) == pos = '@'
            | otherwise = vizchar (HS.member (x, y) walls) (HS.member (x, y) boxes) (HS.member (x - 1, y) boxes)
        vizchar False False False = ' '
        vizchar True False False = '#'
        vizchar False True False = '['
        vizchar False False True = ']'

prep [] = []
prep ('O':xs) = '[':']':prep xs
prep ('@':xs) = '@':' ':prep xs
prep (e:xs) = e:e:prep xs

main = do
        handle <- openFile "day15.txt" ReadMode
        [strmap, strinsns] <- fmap (splitOn "\n\n") (hGetContents handle)
        let insns = parseInsns strinsns
        let origmap = (map prep . lines) strmap
        let width = (length . head) origmap
        let height = length origmap
        let walls = HS.fromList (findNodes origmap '#')
        let boxes = HS.fromList (findNodes origmap '[')
        let [robot] = findNodes origmap '@'
        let (newRobot, newBoxes) = replay insns walls robot boxes
        print (HS.foldr (\(x, y) acc -> acc + (y * 100) + x) 0 newBoxes)
        hClose handle