import System.IO
import Control.Monad
import Data.List (sort, nub)
import Data.Bifunctor (Bifunctor(bimap))

main = do
        handle <- openFile "day1.txt" ReadMode
        answer <- fmap (sum . map (abs . uncurry (-)) . uncurry zip . join bimap sort . unzip . map ((\[a, b] -> (a, b)) . map read . words) . lines) (hGetContents handle) :: IO Integer
        print answer
        hClose handle