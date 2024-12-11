import System.IO
import Control.Monad
import Data.List
import Math.NumberTheory.Logarithms

blink [] = []
blink (a:xs)
    | a == 0 = 1:blink xs
    | odd (integerLog10' a) =
        let log = (integerLog10' a + 1) `div` 2
        in (a `div` (10 ^ log)):(a `mod` (10 ^ log)):blink xs
    | otherwise = a * 2024 : blink xs

blinkN 0 l = l
blinkN n l = blink (blinkN (n - 1) l)

main = do
        handle <- openFile "day11.txt" ReadMode
        answer <- fmap (map read . words) (hGetContents handle)
        print (length (blinkN 25 answer))
        hClose handle