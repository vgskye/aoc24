import System.IO
import Data.List
import Data.List.Split (splitOn)
import Text.Regex.TDFA
import GHC.Base (IO(IO))

pat = "Button A: X\\+([0-9]+), Y\\+([0-9]+)\nButton B: X\\+([0-9]+), Y\\+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)"

linalgStuff [xa, ya, xb, yb, xt, yt] =
    let det = xa * yb - xb * ya in
    let x = (yb * xt) + (-xb * yt) in
    let y = (-ya * xt) + (xa * yt) in
        if x `mod` det /= 0 || y `mod` det /= 0
        then (0, 0)
        else (x `div` det, y `div` det)

prep :: [Integer] -> [Integer]
prep [xa, ya, xb, yb, xt, yt] = [xa, ya, xb, yb, xt + 10000000000000, yt + 10000000000000]

tokens (a, b) = a * 3 + b

main = do
        handle <- openFile "day13.txt" ReadMode
        machines <- fmap (sum . map (tokens . linalgStuff . prep . map read . tail . (\e -> getAllTextSubmatches (e =~ pat))) . splitOn "\n\n") (hGetContents handle)
        print machines
        hClose handle