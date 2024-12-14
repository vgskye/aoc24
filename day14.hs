import System.IO
import Data.List
import Text.Regex.TDFA
pat = "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)"

stepBot w h [x, y, dx, dy] = [(x + dx) `mod` w, (y + dy) `mod` h, dx, dy]

stepBotN w h 1 l = stepBot w h l
stepBotN w h n l = stepBotN w h (n - 1) (stepBot w h l)

quadrant w h (x:y:xs) =
    let halfW = (w - 1) `div` 2 in
    let halfH = (h - 1) `div` 2 in
        if x == halfW || y == halfH
        then -1
        else if x < halfW && y < halfH
        then 0
        else if x > halfW && y < halfH
        then 1
        else if x > halfW && y > halfH
        then 2
        else 3

main = do
        handle <- openFile "day14.txt" ReadMode
        let width = 101;
        let height = 103;
        machines <- fmap (map (stepBotN width height 100 . map read . tail . (\e -> getAllTextSubmatches (e =~ pat))) . lines) (hGetContents handle) :: IO [[Int]]
        let [a, b, c, d] = map (\i -> length (filter (\e -> quadrant width height e == i) machines)) [0..3]
        print (a * b * c * d)
        hClose handle