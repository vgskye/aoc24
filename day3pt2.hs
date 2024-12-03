import System.IO
import Control.Monad
import Data.List
import Text.Regex.TDFA
import Data.List.Split

cons (head:rest) = rest

main = do
        handle <- openFile "day3.txt" ReadMode
        contents <- hGetContents handle
        let splitted = (concatMap ('\n':) . concatMap (cons . splitOn "do()")) (splitOn "don't()" ("do()" ++ contents))
        print splitted
        let matches = getAllTextMatches (splitted =~ "mul\\(([0-9]+),([0-9]+)\\)") :: [String]
        let answer = (sum . map (uncurry (*) . (\[a, b] -> (a, b)) . map (read :: String -> Int) . cons . (\a -> getAllTextSubmatches (a =~ "mul\\(([0-9]+),([0-9]+)\\)") :: [String]))) matches
        print answer
        hClose handle