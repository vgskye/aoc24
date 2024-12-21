import System.IO
import Data.List

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

movePress orig targ = do
    let (x1, y1) = pos orig
    let (x2, y2) = pos targ
    let dx = x1 - x2
    let dy = y1 - y2
    let xm = if dx > 0 then replicate dx '>' else replicate (-dx) '<'
    let ym = if dy > 0 then replicate dy '^' else replicate (-dy) 'v'
    if (x2, y1) == (2, 0)
        then [ym ++ xm ++ "A"]
    else if (x1, y2) == (2, 0)
        then [xm ++ ym ++ "A"]
    else nub [xm ++ ym ++ "A", ym ++ xm ++ "A"]

interleaves [] [] = [[]]
interleaves [] a = [a]
interleaves a [] = [a]
interleaves (a:axs) (b:bxs) = map (a:) (interleaves axs (b:bxs)) ++ map (b:) (interleaves (a:axs) bxs) ++ map ([a,b]++) (interleaves axs bxs) ++ map ([b,a]++) (interleaves axs bxs)

pairs fn [] = []
pairs fn [_] = []
pairs fn (a:b:xs) = fn a b:pairs fn (b:xs)

trimLast [_] = []
trimLast (a:xs) = a:trimLast xs

variations [a] = a
variations (a:xs) = concatMap (\e -> map (++ e) a) (variations xs)

main = do
    handle <- openFile "day21.txt" ReadMode
    codes <- fmap lines (hGetContents handle)
    let indirect = variations . pairs movePress . ('A':)
    let control = minimum . map length . concatMap indirect . concatMap indirect . indirect
    let numerical = read . trimLast
    let solve a = numerical a * control a
    print (sum $ map solve codes)
    hClose handle