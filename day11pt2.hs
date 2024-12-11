import System.IO
import Control.Monad
import Data.List
import Math.NumberTheory.Logarithms
import Data.MemoTrie
import Control.Parallel.Strategies (parMap, rpar)
import Data.Int (Int64)

l10 = integerLog10' . toInteger

blinked a
    | a == 0 = [1]
    | odd (l10 a) =
        let log = (1 + l10 a) `div` 2
        in [a `div` (10 ^ log), a `mod` (10 ^ log)]
    | otherwise = [a * 2024]

blinkcount 0 _ = 1
blinkcount n i = (sum . parMap rpar (mblinkcount (n - 1)) . blinked) i

mblinkcount :: Int -> Int64 -> Int64
mblinkcount = memo2 blinkcount
main = do
        handle <- openFile "day11.txt" ReadMode
        answer <- fmap (map read . words) (hGetContents handle)
        print (sum (map (mblinkcount 75) answer))
        hClose handle