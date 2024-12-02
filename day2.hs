import System.IO
import Control.Monad
import Data.List

isSafeInner :: [Int] -> Bool
isSafeInner [] = True
isSafeInner [a] = True
isSafeInner (a:b:xs) =
    let diff = b - a
    in 1 <= diff && diff <= 3 && isSafeInner (b:xs)

isSafe :: [Int] -> Bool
isSafe l
    | head l > l !! 1 = isSafeInner (reverse l)
    | otherwise = isSafeInner l

main = do
        handle <- openFile "day2.txt" ReadMode
        answer <- fmap (length . filter isSafe . map (map read . words) . lines) (hGetContents handle)
        print answer
        hClose handle