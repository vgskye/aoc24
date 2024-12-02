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

removeAt xs n =  let (ys,zs) = splitAt n xs in ys ++ tail zs

isSafeWithout :: [Int] -> Int -> Bool
isSafeWithout l idx = isSafe (removeAt l idx)

isReallySafe :: [Int] -> Bool
isReallySafe l = any (isSafeWithout l) [0..(length l - 1)]

main = do
        handle <- openFile "day2.txt" ReadMode
        answer <- fmap (length . filter isReallySafe . map (map read . words) . lines) (hGetContents handle)
        print answer
        hClose handle