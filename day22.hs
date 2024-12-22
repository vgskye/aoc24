import System.IO
import Data.Bits

step :: Int -> Int
step n = do
    let i1 = ((n `shiftL` 6) `xor` n) `mod` 16777216
    let i2 = ((i1 `shiftR` 5) `xor` i1) `mod` 16777216
    let i3 = ((i2 `shiftL` 11) `xor` i2) `mod` 16777216
    i3

stepN :: Int -> Int -> Int
stepN 0 a = a
stepN n a = step (stepN (n - 1) a)

main = do
    handle <- openFile "day22.txt" ReadMode
    codes <- fmap (map read . lines) (hGetContents handle) :: IO [Int]
    print (sum (map (stepN 2000) codes))
    hClose handle