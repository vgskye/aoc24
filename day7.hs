import System.IO
import Control.Monad
import Data.List
import Data.List.Split

parse :: [String] -> (Integer, [Integer])
parse [target, numbers] = (read target, (map read . words) numbers)

genAll [x] = [x]
genAll (cur:rest) = map (cur *) (genAll rest) ++ map (cur +) (genAll rest)

isOkay (target, numbers) = target `elem` (genAll . reverse) numbers



main = do
        handle <- openFile "day7.txt" ReadMode
        rules <- fmap (map (parse . splitOn ": ") . lines) (hGetContents handle)
        print ((sum . map fst . filter isOkay) rules)
        hClose handle