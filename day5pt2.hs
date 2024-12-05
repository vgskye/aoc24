import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

isOkay (Just a) (Just b) = a < b
isOkay _ Nothing = True
isOkay Nothing _ = True

sorter rules a b
        | [a, b] `elem` rules = GT
        | [b, a] `elem` rules = LT
        | otherwise = EQ

dothMatch [] l = True
dothMatch ([a, b]:xs) l = isOkay (elemIndex a l) (elemIndex b l) && dothMatch xs l

middle l = l !! div (length l - 1) 2

main = do
        handle <- openFile "day5.txt" ReadMode
        [rules, queers] <- fmap (splitOn [""] . lines) (hGetContents handle)
        let parsed = map (map read . splitOn "|") rules :: [[Int]]
        let gays = map (map read . splitOn ",") queers :: [[Int]]
        let matches = (sum . map (middle . sortBy (sorter parsed)) . filter (not . dothMatch parsed)) gays
        print matches
        hClose handle