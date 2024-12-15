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

findAir dir boxes (x, y)
    | (x, y) `HS.member` boxes = findAir dir boxes (nextPos (x, y) dir)
    | otherwise = (x, y)

pultimush walls (x, y) dir boxes
    | findAir dir boxes (nextPos (x, y) dir) `HS.member` walls = ((x, y), boxes)
    | findAir dir boxes (nextPos (x, y) dir) == nextPos (x, y) dir = (nextPos (x, y) dir, boxes)
    | otherwise = (nextPos (x, y) dir, HS.insert (findAir dir boxes (nextPos (x, y) dir)) (HS.delete (nextPos (x, y) dir) boxes))

replay [] walls pos boxes = (pos, boxes)
replay (dir:xs) walls pos boxes =
    let (newPos, newBoxes) = pultimush walls pos dir boxes in
        replay xs walls newPos newBoxes

vizmap w h walls boxes = concatMap (vizrow w walls boxes) [0..h - 1]
    where
        vizrow w walls boxes y = '\n':map (vizpoint walls boxes y) [0..w - 1]
        vizpoint walls boxes y x = vizchar (HS.member (x, y) walls) (HS.member (x, y) boxes)
        vizchar False False = ' '
        vizchar True False = '#'
        vizchar False True = 'O'

main = do
        handle <- openFile "day15.txt" ReadMode
        [strmap, strinsns] <- fmap (splitOn "\n\n") (hGetContents handle)
        let insns = parseInsns strinsns
        let origmap = lines strmap
        let width = (length . head) origmap
        let height = length origmap
        let walls = HS.fromList (findNodes origmap '#')
        let boxes = HS.fromList (findNodes origmap 'O')
        let [robot] = findNodes origmap '@'
        let (_, newBoxes) = replay insns walls robot boxes
        print (HS.foldr (\(x, y) acc -> acc + (y * 100) + x) 0 newBoxes)
        hClose handle