import System.IO
import Control.Monad
import Data.List
import Math.NumberTheory.Logarithms
import Debug.Trace
import Data.Function.Memoize
import Control.Parallel.Strategies (parMap, rpar)

blinked a
    | a == 0 = [1]
    | odd (integerLog10' a) =
        let log = (1 + integerLog10' a) `div` 2
        in [a `div` (10 ^ log), a `mod` (10 ^ log)]
    | otherwise = [a * 2024]

blinkcount _ 1 l = (length . concatMap blinked) l
blinkcount self n l = (sum . parMap rpar (self (n - 1) . blinked)) l

mblinkcount :: Int -> [Integer] -> Int
mblinkcount = memoFix2 blinkcount

main = do
        handle <- openFile "day11.txt" ReadMode
        answer <- fmap (map read . words) (hGetContents handle)
        print (mblinkcount 75 answer)
        hClose handle