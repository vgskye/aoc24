import System.IO
import Control.Monad
import Data.List (sort, nub)
import Data.Bifunctor (Bifunctor(bimap))


main = do
        handle <- openFile "day1.txt" ReadMode
        (first, second) <- fmap (unzip . map ((\[a, b] -> (a, b)) . map read . words) . lines) (hGetContents handle) :: IO ([Int], [Int])
        print (sum (map (\x -> x * length (filter (x ==) second)) first))
        hClose handle