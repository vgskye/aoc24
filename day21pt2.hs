import System.IO
import Data.MemoTrie
import Control.Parallel.Strategies (rpar, parMap)

pos 'A' = (0, 0)
pos '^' = (1, 0)
pos '0' = (1, 0)
pos '>' = (0, 1)
pos '3' = (0, -1)
pos 'v' = (1, 1)
pos '2' = (1, -1)
pos '<' = (2, 1)
pos '1' = (2, -1)
pos '6' = (0, -2)
pos '5' = (1, -2)
pos '4' = (2, -2)
pos '9' = (0, -3)
pos '8' = (1, -3)
pos '7' = (2, -3)

movePressI orig targ = do
    let (x1, y1) = pos orig
    let (x2, y2) = pos targ
    let dx = x1 - x2
    let dy = y1 - y2
    let xm = if dx > 0 then replicate dx '>' else replicate (-dx) '<'
    let ym = if dy > 0 then replicate dy '^' else replicate (-dy) 'v'
    -- map (++ "A") (nub $ interleaves xm ym)
    if dx == 0
        then [ym ++ "A"]
    else if dy == 0
        then [xm ++ "A"]
    else if (x2, y1) == (2, 0)
        then [ym ++ xm ++ "A"]
    else if (x1, y2) == (2, 0)
        then [xm ++ ym ++ "A"]
    else [xm ++ ym ++ "A", ym ++ xm ++ "A"]

movePress = memo2 movePressI

pairs fn [] = []
pairs fn [_] = []
pairs fn (a:b:xs) = fn a b:pairs fn (b:xs)

trimLast [_] = []
trimLast (a:xs) = a:trimLast xs

ropeI 0 orig targ = length . head $ movePress orig targ
ropeI n orig targ = minimum $ parMap rpar (sum . pairs (rope (n - 1)) . ('A':)) (movePress orig targ)

rope :: Int -> Char -> Char -> Int
rope = memo3 ropeI

main = do
    handle <- openFile "day21.txt" ReadMode
    codes <- fmap lines (hGetContents handle)
    let control = sum . pairs (rope 25) . ('A':)
    let numerical = read . trimLast
    let solve a = numerical a * control a
    print (sum $ parMap rpar solve codes)
    hClose handle